# Sphinx Usage Guide - CMAQ Documentation

**Edited** from the [CRACMM Sphinx Usage Guide](https://github.com/USEPA/CRACMM/blob/CRACMMdocs/sphinx/README.md)

This page provides documentation for the process of using Sphinx to build HTML for the CRACMM website. It is primarily intended for CRACMM developers at the EPA. Others may or may not find the information presented here useful.

[Sphinx](https://www.sphinx-doc.org/en/master/) is a software written in Python that is used to create HTML files based on already existing files. It is typically used to create websites that document software and it can be used in combination with GitHub's [Pages](https://pages.github.com/) utility to publish a website based on a GitHub repository. This page explains how Sphinx is used to create a GitHub site for the CRACMM repository using Sphinx.

To view this page on on the CRACMM website instead of in raw markdown (if you're not already there), click [here](https://USEPA.github.io/CRACMM/sphinx/README.html).


## Table of Contents
* [Git Workflow Summary](#git-workflow-summary): This section goes over the basic git workflow for updating HTML on the CRACMM GitHub Pages website. If you are looking for detailed instructions on this process, this is not the tutorial you are looking for. Use the "Repository Setup" and "Process to Build HTML" tutorials instead.

* [Environment Setup](#environment-setup): This section provides instructions on how to create a Python environment designed to run Sphinx on the CRACMM repository.