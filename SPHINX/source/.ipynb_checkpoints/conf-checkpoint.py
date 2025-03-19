# Configuration file for the Sphinx documentation builder.
#
# For the full list of built-in configuration values, see the documentation:
# https://www.sphinx-doc.org/en/master/usage/configuration.html

# -- Project information -----------------------------------------------------
# https://www.sphinx-doc.org/en/master/usage/configuration.html#project-information

## Project Details - Shows on Hyperlink
project = 'CMAQ'
## not showing 'copyright' on website
copyright = 'license.md'
author = 'USEPA CMAQ Developers'
release = '5.5+'

# -- General configuration ---------------------------------------------------
# https://www.sphinx-doc.org/en/master/usage/configuration.html#general-configuration
extensions = ['myst_parser',                  # Needed for parsing markdown, enables myst syntax
        'sphinx.ext.githubpages',             # Automatically loads .nojekyll - still need to do so manually into docs/
        'sphinx_design',                      # Enables design directive objects (cards, grids)
        'pydata_sphinx_theme',                # Project Theme
        'sphinx.ext.intersphinx',             # Allows for cooperation with other sphinx projects, possibly Github
        'sphinx.ext.autodoc',                 # Creates Automated documentation versions of non-.md, .txt, .rst files
        'sphinx.ext.autosectionlabel',        # Automated Label library for poorly structured documents - sidebar
        'sphinx_new_tab_link',                 # Opens new tabs for external (non-.md) links
        'sphinx_favicon',
             ]

## enables extension
autosectionlabel_prefix_document=True
## enables .md colon fence syntax
myst_enable_extensions = ['colon_fence']
## Something to do with dropdowns, some sidebar options but not this configuration
myst_heading_anchors = 1
## No templates used but standard
templates_path = ['_templates']
## Do not create documentation from these file types and directories
exclude_patterns = ['_build', 'Thumbs.db', '.DS_Store', '**.ipynb_checkpoints']
## How myst parses file types
source_suffix = {
        '.rst': 'restructuredtext',
        '.txt': 'markdown',
        '.md': 'markdown'
        }
favicons = ['CMAQ_Logo_2_inch.png']

# -- Options for HTML output -------------------------------------------------
# https://www.sphinx-doc.org/en/master/usage/configuration.html#options-for-html-output
import pydata_sphinx_theme
html_theme = 'pydata_sphinx_theme'
# external link icon
new_tab_link_show_external_link_icon = True
## Vector graphics and icon logos stored here
html_static_path = ['_static']

## Anchor to github pages url
html_baseurl = 'https://jbrunto.github.io/CMAQ_Docs_v55/'

## No sidebars on home page, left sidebar on all other pages
html_sidebars = {'index': [],
        '**': ['sbt-sidebar-nav.html']}

## Theme Specific HTML options - see Theme documentation
html_theme_options = {
    ## github icon on navbar
    "github_url": "https://github.com/USEPA/CMAQ",
    ## Logo on navbar, operates as home button
    "logo": {
        "image_light": "_static/CMAQ_Logo_2_inch.png",
        "image_dark": "_static/CMAQ_Logo_2_inch.png"
        },
    ## Dropdown stuff, probably not applicable to our build but is stable
    #"navigation_depth": 2,
    ## Navbar icons - all arguments needed
    "icon_links": [
        {
        "name": "CMAQ - EPA Site" ,
        "url": "https://www.epa.gov/cmaq",
        "icon": "_static/epa_seal_w-ring_RGB-sm.png",
        "type": "local"
        },
        {
        "name": "User Forum",
        "url": "https://forum.cmascenter.org/",
        "icon": "_static/cmas_logo.png",
        "type": "local"
        },
        {
        "name": "CRACMM",
        "url": "https://usepa.github.io/CRACCM/",
        "icon": "_static/CRACMM_1.png",
        "type": "local"
        }
    ],
    "secondary_sidebar_items": ['page-toc'],
    #"show_nav_level": 1,
    "show_toc_level": 1,
    #"header_links_before_dropdown": 1,
    ## Navbar options
    "navbar_start": [],
    "navbar_center": ['navbar-logo'],
    "navbar_end": ['theme-switcher', 'navbar-icon-links'],
    "navbar_persistent": ['search-button-field'],

    
}

html_show_copyright = False