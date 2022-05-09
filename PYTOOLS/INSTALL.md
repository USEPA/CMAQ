Installation
============

The CMAQ Python Tools require Python version 3.6 (or greater) and additional
libraries for processing data efficiently. Instructions on installation are
provided below, but any standard installation processes are acceptable
alternatives.

The best way to test your installation is to run `python show_versions.py`

Installing Python >=3.6
-----------------------

Installing Python >=3.6 can be done by installing using the installer for your
system from python.org, or you can use a package like Anaconda.

Installing Libraries
--------------------

Each tool will have some basic library requirements and many will share the 
same requirements. As a result, each tools should have it's own 
`./requirements.txt` and this folder contains a general `requirements.txt` that
will meet the needs of many tools.

To install the requirements of a particular tool, you can use pip or conda.

pip
~~~

`pip install --user -r requirements.txt`

To confirm that you have the libraries installed correctly, run

`python show_versions.py requirements.txt`

Anaconda
~~~~~~~~

`conda env create -f environment.yml`
