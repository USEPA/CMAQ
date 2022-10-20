# Installation

The CMAQ Python Tools require Python version 3.6 (or greater) and additional
libraries for processing data efficiently. Instructions on installation are
provided below, but any standard installation processes are acceptable
alternatives.

The best way to test your installation is to run `python show_versions.py`

## Installing Python >=3.6

Installing Python >=3.6 can be done by using the installer for your
system from python.org, or you can use a package like Anaconda.

## Virtual Environment (optional)

If you don't want changes to affect your user or system environment, create
a virtual environment to isolate the CMAQ python tools installation.

```
# Create a new virtual environment
python -m venv ./cmaqpy
# Do this anytime to activate the new environment
source ./cmaqpy/bin/activate
# Do this anytime to deactivate the new environment
deactivate
```

If you're making a virtual environment and your python is from Anaconda,
the ensurepip module will be missing and the `python -m venv ./cmaqpy`
command will fail. Usually, Anaconda does not use venv but instead uses
conda env. If you do want to use venv with Anaconda, modify the above 
as follows:

```
# Create a new virtual environment
python -m venv ./cmaqpy
# Do this anytime to activate the new environment
source ./cmaqpy/bin/activate
# Install pip
curl https://bootstrap.pypa.io/get-pip.py -o get-pip.py
python get-pip.py
# Do this anytime to deactivate the new environment
deactivate
```

If your python version is not "current", then get-pip.py will tell you which
file to download instead.


## Installing Libraries

Each tool will have some basic library requirements and many will share the 
same requirements. As a result, each tools should have it's own 
`./requirements.txt` and this folder contains a general `requirements.txt` that
will meet the needs of many tools.

To install the requirements of a particular tool, you can use pip or conda.


### pip

`pip install --user --prefer-binary -r requirements.txt`

To confirm that you have the libraries installed correctly, run

`python show_versions.py requirements.txt`


### Anaconda

Start by installing Anaconda or Miniconda. To install custom libraries, run
the command below. If you started with Miniconda, it will take longer to run.

`conda env create -n cmaqpy -f environment.yml`

To activate the environment, run

`conda activate cmaqpy`

