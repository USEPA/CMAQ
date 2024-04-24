__doc__ = """
Overview
========

Utility to create CMAQ-ready files from shapefiles or other vector format.
Although it has only been tested with shapefiles, it should support geojson.

Contents
--------

shp2cmaq : function
    Converts a shapefile to a CMAQ-ready mask file using area overlap

Updates:
- v1.0 : Barron H. Henderson converted code to a script.


Installation Instructions
-------------------------

Requires Python3 and libraries cmaqsatproc, geopandas, xarray, netcdf4.

.. code-block::bash

    python -m pip install cmaqsatproc geopandas xarray netcdf4


Example Application
-------------------

1. Download shapefile
    https://www2.census.gov/geo/tiger/GENZ2022/shp/cb_2022_us_state_500k.zip
2. Run tool:

.. code-block::bash

    python shp2cmaq.py cb_2022_us_state_500k.zip STUSPS 12US1

.. code-block::python

    # shp2cmaq file should be in run directory
    import shp2cmaq
    shp2cmaq.shp2cmaq('cb_2022_us_state_500k.zip', 'STUSPS', '12US1')

"""
import os
import string


__version__ = '1.0'


def shp2cmaq(
    shppath, attrkey, gdnam, gdpath=None, outpath=None, bcrs='EPSG:4269',
    srckey='intersection_area', outformat='NETCDF4_CLASSIC', prefix=None,
    overwrite=False, verbose=1
):
    """
    Arguments
    ---------
    shppath : str
        Path to a shapefile or zip file containing a shapefile
    attrkey : str
        Column to group shapes by e.g., STUSPS of census (AL, NC, etc)
    gdnam : str
        Name of grid definition within gdpath (e.g., 12US1, 108NHEMI2)
    gdpath : str
        Path to a GRIDDESC file (optional for EPA domains 1US1, 4US1, 12US1,
        12US2, 36US3, 108NHEMI2, etc)
    outpath : str
        Defines where the output will be stored.
    outformat : str
        Either NETCDF4_CLASSIC or NETCDF3_CLASSIC (bigger, more compatible)
    prefix : str
        Variable names with be <prefix>_<attrvalue> (STUSPS_NC)
    srckey : str
        intersection_area, uses the area overlap.
        Anything else, uses srckey * intersection_area / shape_area
    overwrite : bool
        If True, overwrite without asking.
    verbose : int
        Level of verbosity
    """
    import cmaqsatproc as csp
    import geopandas as gpd

    if prefix is None:
        prefix = f'{attrkey}_'
    prefix = prefix.upper()

    if outpath is None:
        if isinstance(shppath, str):
            rootname = os.path.splitext(os.path.basename(shppath))[0]
            outpath = f'./{rootname}.{attrkey}.{gdnam}.IOAPI.nc'
        else:
            raise ValueError('outpath is required when shppath is not a string.')

    if os.path.exists(outpath):
        if overwrite:
            print(f'INFO: Overwriting {outpath}.')
        else:
            print(f'Using cached {outpath}; set overwrite=True to remake.')
            return outpath

    if verbose > 0:
        print(f'INFO: Running this will create {outpath}.')

    gf = csp.open_griddesc(gdnam, gdpath=gdpath)
    if gf.GDTYP == 6:
        # For polar stereographic, make sure the bounds are reasonable.
        bbox = (-181, -30, 181, 91)
    else:
        # The default bounding box is the perimiter in WGS84 + 1 degree buffer
        # You may need to update 4326 to match your shapefile
        bbox = gf.csp.geodf.envelope.to_crs(
            bcrs
        ).unary_union.envelope.buffer(1).bounds

    if isinstance(shppath, str):
        shpf = gpd.read_file(shppath, bbox=bbox)
    elif isinstance(shppath, gpd.GeoDataFrame):
        shpf = shppath
    else:
        raise TypeError(
            'shppath must be a path to a polygon file or a geopandas.'
            + f'GeoDataFrame; got {type(shppath)}'
        )
    if srckey != 'intersection_area':
        if srckey not in shpf.columns:
            raise KeyError(f'Cannot find {srckey}; found {shpf.columns}')

    if attrkey not in shpf.columns:
        print('WARNING: attrkey must be in or derived from columns:')
        print(sorted(shpf.columns))

    if bcrs != shpf.crs:
        print(f'INFO: Bounding box in {bcrs} and shapefile in {shpf.crs}.')
        print('INFO: If both are lat/lon variants, this is okay.')
        print(f'INFO: You can set bcrs=\'{shpf.crs}\' and run again')

    # Reproject input as grid projection and add custom variable with
    # basic name cleanup
    dshpf = shpf.to_crs(gf.csp.geodf.crs)
    attrdefn = f'{attrkey} removing spaces and special characters'
    # Derive custom key from another existing key
    dshpf['custom'] = prefix + dshpf[attrkey].astype(str)
    # Cleanup names using a few common rules
    dshpf['custom'] = dshpf['custom'].str.upper().str.replace('-', '_')
    dshpf['custom'] = dshpf['custom'].str.replace('~', '_')
    dshpf['custom'] = dshpf['custom'].str.replace('.', '_')
    dshpf['custom'] = dshpf['custom'].str.replace('_99', 'UNK')
    # add your own additional cleanup
    # dshpf['custom'] = dshpf['custom']...

    # Warn about weird names
    allowed_ascii = string.ascii_uppercase + string.digits + '_'
    anywarning = 0
    for cat in dshpf['custom']:
        if len(cat) > 15:
            print(f'WARNING: {cat} is longer than 15 characters.')
            anywarning += 1
        if cat[:1] not in string.ascii_uppercase:
            print(f'WARNING: {cat} starts with non uppercase ascii.')
            anywarning += 1
        for c in cat:
            if c not in allowed_ascii:
                print(f'WARNING: {cat} has unallowed characters.')
                anywarning += 1
                break
    if anywarning > 0:
        msg = 'INFO: Using names longer than 15 characters, with non-uppercase'
        msg += ' characters, or having special characters can cause issues'
        msg += ' when used with CMAQ. To shorten names, try adding a'
        msg += ' shorter than the attrkey. Or make a custom attribute'
        msg += ' that is shorter before processing.'
        print(msg)
        msg = 'EXAMPLE: Trim off first 7 characters of geoid\n'
        msg += '    import geopandas\n'
        msg += '    from shp2cmaq import shp2cmaq\n\n'
        msg += '    shppath = "acs2022_5yr_B01003_04000US21.shp"'
        msg += '    shpf = gpd.read_file(shppath)\n'
        msg += '    shpf["shortname"] = shpf["geoid"].str[7:]\n'
        msg += '    shp2cmaq(shpf, "shortname", "36US3", srckey="B01003001")'
        print(msg)
        
    # only required if calculating weighted values
    dshpf['shape_area'] = dshpf.geometry.area

    if verbose > 0:
        print('Unique Labels:')
        print(dshpf['custom'].unique())

    # Calculate variables for each unique attribute value for fractional
    # area overlap, the dominant value
    domkey = f'{prefix}DOM'
    totkey = f'{prefix}TOT'

    attr_area = gpd.overlay(
        dshpf, gf.csp.geodf.reset_index(), how='intersection'
    )
    intersection_area = attr_area.geometry.area
    if srckey == 'intersection_area':
        attr_area[srckey] = intersection_area
    else:
        # Add a intersection area weighted attribute value (e.g., population)
        weight = intersection_area / attr_area['shape_area']
        attr_area[srckey] = weight * attr_area[srckey]

    attrkeys = sorted(attr_area['custom'].unique())
    attr_area['custom_idx'] = attr_area['custom'].apply(attrkeys.index)
    gbkeys = ['custom', 'ROW', 'COL']
    attr_area_sum = attr_area.groupby(gbkeys).sum(numeric_only=True)

    dom_area = attr_area.sort_values(
        by=srckey, ascending=False
    ).groupby(['ROW', 'COL']).agg(**{
        'custom': ('custom', 'first'), domkey: ('custom_idx', 'first'),
        totkey: (srckey, 'sum')
    })
    dom_area_ds = dom_area.to_xarray()
    catds = attr_area_sum[srckey].unstack('custom').fillna(0).to_xarray()

    catds[totkey] = dom_area_ds[totkey]

    # Add all variables to gf
    for vark, var in catds.data_vars.items():
        print(vark, end=',', flush=True)
        gf[vark] = var.astype('f')
        var_desc = f'Fractional overlap of {vark} with {gdnam}'
        gf[vark].attrs.update(
            long_name=vark.ljust(16),
            var_desc=var_desc.ljust(80)[:80],
            unit='1'.ljust(16)
        )

    # Set any missing values (i.e., no overla) to 0.
    for vark in gf.data_vars:
        gf[vark] = gf[vark].fillna(0)

    # Add dominant key variable with custom missing value.
    gf[domkey] = dom_area_ds[domkey].fillna(-999).astype('i')
    gf[domkey] = gf[domkey].fillna(-999).astype('i')
    desctxt = str({k: i for i, k in enumerate(attrkeys)})[:-1]
    desctxt += ", 'UNASSIGNED': -999}"
    gf[domkey].attrs.update(
        long_name=domkey.ljust(16),
        var_desc=f'Dominant {attrkey} for {gdnam} cell'.ljust(80)[:80],
        units='1'.ljust(16), description=desctxt
    )

    # Add IOAPI meta-data
    igf = gf.expand_dims(TSTEP=1, LAY=1).csp.to_ioapi()
    desctxt = f'{attrkey} fractional area coverage, total ({prefix}TOT) and'
    desctxt += f' dominant ({prefix}DOM)'
    igf.attrs['FILEDESC'] = f"""title: {outpath}
    author: shapefile2cmaq
    description: {desctxt}
    inputs:
     - Shapefile: {shppath}
     - Attribute: {attrdefn}
     - GDNAM: {gdnam}
     - GDPATH: {gdpath}
    file_version: 1.0
    tool_version: {__version__}
    """.ljust(80*60)[:60*80]
    outdir = os.path.dirname(outpath)
    if outdir != '':
        os.makedirs(outdir, exist_ok=True)
    igf.to_netcdf(outpath, format=outformat)
    return outpath


if __name__ == '__main__':
    import argparse
    prsr = argparse.ArgumentParser()
    aa = prsr.add_argument
    hstr = 'level of verbosity'
    aa('--verbose', help=hstr, default=0, action='count')
    hstr = 'Overwrite existing output'
    aa('--overwrite', help=hstr, default=False, action='store_true')
    hstr = 'NETCDF4_CLASSIC (smaller) or NETCDF3_CLASSIC (more compatible)'
    aa('--outformat', help=hstr, default='NETCDF4_CLASSIC')
    hstr = 'Variable names with be <prefix>_<attrvalue>'
    aa('--prefix', help=hstr, default=None)
    hstr = 'Anything other than intersection_area, uses srckey'
    hstr += ' * intersection_area / shape_area'
    aa('--srckey', help=hstr, default='intersection_area')
    aa('--gdpath', help='GRIDDESC path', default=None)
    aa('shppath', help='Path to shapefile or zip containing shapefile')
    aa('attrkey', help='Attribute in shapefile')
    aa('GDNAM', help='Grid definition name (must be in gdpath)')
    aa('outpath', nargs='?', default=None)
    args = prsr.parse_args()
    shp2cmaq(vars(args))
