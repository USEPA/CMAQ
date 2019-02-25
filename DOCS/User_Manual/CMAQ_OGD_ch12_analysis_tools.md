<!-- BEGIN COMMENT -->

[<< Previous Chapter](CMAQ_OGD_ch11_code_management.md) - [Home](README.md) - [Next Chapter >>](CMAQ_OGD_ch13_support.md)

<!-- END COMMENT -->

# Analysis Tools for CMAQ #
** >>COMMENT<< ** Recommend shortening this section. Remove the summaries of the software programs that are a mix of technical and big picture descriptions.  For each tool just provide a brief summary (in list form) of the tasks that the software can be used to perform.

** >>COMMENT<< ** Delete PAVE section; move the AMET subsection up behind VERDI and emphasize tool much more than the others and provide more specifics on how to run AMET similar to the detail for ICON/BCON. Suggested order: VERDI, AMET, CMAQ tools (hr2day, etc.), then all the other stuff like NCO and IDL.

** >>COMMENT<< ** Add new Fortran utility "calc_tmetric", add Barron's PseudoNetCDF

Several tools are freely available for visualizing, analyzing, and evaluating CMAQ inputs and outputs. The list includes CMAQ utility tools, m3tools, PAVE, VERDI, Panoply, the Atmospheric Model Evaluation Tool (AMET), netCDF Operators (NCO), UNIDATA Integrated Data Viewer (IDV), and the NCAR Command-line Language (NCL). Several other commercial packages, including MATLAB and IDL, also support the analysis and visualization of CMAQ inputs and outputs. Most visualization and analysis software that supports netCDF file formats will work with CMAQ output data.

This page briefly describes several of these software utilities and provides links to where they may be downloaded. Refer to the documentation for each piece of software for additional information.

Two classes of analysis tools are presented here

- [Command line utilities](#cmdtools) for manipulating CMAQ input/output data
  - [netCDF](#netcdf)
  - [appendwrf](#post_tools)
  - [bldoverlay](#post_tools)
  - [block_extract](#post_tools)
  - [combine](#post_tools)
  - [hr2day](#post_tools)
  - [sitecmp](#post_tools)
  - [sitecmp_dailyo3](#post_tools)
  - [writesite](#post_tools)
  - [m3tools](#m3tools)
  - [netCDF Operators](#nco)


- [Visualization tools](#viztools) for graphical display of CMAQ input/output data
  - [VERDI](#verdi)
  - [AMET](#amet)
  - [PAVE](#pave)
  - [IDV](#idv)
  - [NCL](#ncl)


  -----------

Command Line Data Processors
-------------

<a id="netcdf"><a/>

### netCDF


<a id="cmdtools"><a/>

### CMAQ Utility Tools



<a id="m3tools"><a/>

### M3tools

<a id="nco"><a/>

### netCDF Operators (NCO)

<a id="viztools"><a/>

Visualization Tools
-------------------------------
<a id="verdi"></a>



### Visualization Environment for Rich Data Interpretation (VERDI)

<a id="amet"><a/>

### Atmospheric Model Evaluation Tool (AMET)


<a id="pave"><a/>

### Package for Analyses and Visualization of Environmental Data (PAVE)

[http://paved.sourceforge.net](http://paved.sourceforge.net/)

PAVE is a flexible and distributed application to visualize multivariate gridded environmental datasets. Features include

-   baseline graphics with the option to export data to high-end commercial packages
-   the ability to access and manipulate datasets located on remote machines
-   support for multiple simultaneous visualizations
-   an architecture that allows PAVE to be controlled by external processes
-   low computational overhead
-   no software distribution cost

PAVE is very widely used by the air quality modeling community, and it can produce various types of plots, including scatter plots, time-series plots, 2-D tile plots, 3-D surface plots, bar plots, wind-vector plots, etc. The source code for PAVE is also distributed under the terms of the GNU General Public License Version 2. PAVE can be run at the Linux command prompt, and the various commands/options can be invoked using the graphical user interface (GUI), or all of them can be stored in a script file and executed by running the script. However, note that PAVE is not being updated any more, and CMAS has ceased support for PAVE, and encourages the user community to move towards VERDI (discussed next).

<a id="idv"><a/>

### Integrated Data Viewer (IDV)

<a id="ncl"><a/>

## NCAR Command Language (NCL)
[http://www.ncl.ucar.edu](http://www.ncl.ucar.edu/)

The NCAR Command Language (NCL) is a free, interpreted language designed specifically for scientific data processing and visualization. NCL has robust file input and output. It can read in netCDF, HDF4, HDF4-EOS, GRIB, binary, and ASCII data. The output graphics from NCL are of high quality, and highly customizable.

It runs on many different operating systems, including Solaris, AIX, IRIX, Linux, MacOSX, Dec Alpha, and Cygwin/X running on Windows. It is available for free in binary format.

NCL can be run in interactive mode, where each line is interpreted as it is entered at the user’s workstation, or it can be run in batch mode as an interpreter of complete scripts. The user can also use command-line options to set options or variables on the NCL command line.

The power and utility of the language are evident in three areas:

-   file input and output
-   data analysis
-   visualization

NCL has many features common to modern programming languages, including types, variables, operators, expressions, conditional statements, loops, and functions and procedures. The various NCL commands can be executed at the NCL command prompt, or all the commands can be stored in a file and invoked as follows:

ncl commands.ncl

NCL comes with numerous predefined functions and resources that the user can easily invoke. These include functions for data processing, visualization, and various mathematical and statistical analyses, such as empirical orthogonal functions (EOFs) and singular value decomposition (SVD). As an example, contributed.ncl is a library of user-contributed functions within NCL. This library is distributed with NCL, and loading the script at the beginning of the user’s NCL script therein can access the functions.

`load "$NCARG_ROOT/lib/ncarg/nclscripts/csm/contributed.ncl"`

NCL also has a capability to call external Fortran or C routines. This is enabled through an NCL wrapper script called WRAPIT. The wrapper file is a C program that describes all of the arguments and passes them back and forth between the function/procedure the user wants to call, and the NCL script that is calling it. Thus, when the user invokes WRAPIT on the Fortran code that needs to be called from NCL, it creates a special C wrapper file, compiles it and the Fortran file, and generates a \*.so file that the user can then load into NCL using the "external" statement.

<!-- BEGIN COMMENT -->

[<< Previous Chapter](CMAQ_OGD_ch11_code_management.md) - [Home](README.md) - [Next Chapter >>](CMAQ_OGD_ch13_support.md)<br>
CMAQ Operational Guidance Document (c) 2016<br>

<!-- END COMMENT -->
