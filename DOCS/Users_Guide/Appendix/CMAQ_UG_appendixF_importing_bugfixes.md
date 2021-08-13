<!-- BEGIN COMMENT -->

[<< Previous Appendix](CMAQ_UG_appendixE_configuring_WRF.md) - [Home](../README.md)

<!-- END COMMENT -->

# Appendix F: Importing Bugfixes from CMAQ github page 

## F.1 Importing Bugfixs from the issues page to your local space

* Users importing bugfixes from the CMAQ github issues page have a couple of options depending on how the user has configured their CMAQ repo local. The first option involves porting the change to the users github repo or source code repo, while the second option ports the change to the users CMAQ project. The CMAQ team recommends the first option as this will allow users to configure and build the model a number of times with the bugfix. 

## F.2 Option 1: Importing bugfix to your local github repo or source code repo 

* To import the bugfix to your local github repo/source code repo, please note the following before following the steps below: 

  - This method will require you to rebuild your individual CMAQ projects for the effect to take place. Therefore, if you have made changes to the source code in your individual CMAQ projects this method will not preserve those changes when you make that change. 
  
  - This method will not preserve any changes made to the source code you have modified in your local github or source code repo. Ex. If a specific change is made to the chemistry solver source code (e.g. hrsolver.F), and you are porting in the new code that contains a bugfix in (e.g. hrsolver.F) the specific changes made will be overridden. 
  
* Steps to porting bugfixs to your local github repo or source code directory:

  - Go to the [CMAQ Known Issues page](https://github.com/USEPA/CMAQ/issues) and click on your issue.
  
  - Identify which files require updating by looking at the solution, a link to the file(s) should be posted. Click on the link.
  
  - You should now be in the specific issue page, under the Known_Issues folder. Here you should be able to see all the files that need to be updated.
  
  - Download these files to your system storing them in a scratch folder, for those on Unix Systems the following commands can be used:

```
wget https://raw.githubusercontent.com/USEPA/CMAQ/main/DOCS/Known_Issues/CMAQv5.3.1-i#/your_files .

```
  - Once you have the updated versions of bugfixed files, you need to find where these files are located in your source code, so you can replace them. Most often, these files will be located in one of the CCTM/src folder within the CMAQ repo. You will then just go to where that file is located and simple replace that file with the bugfixed file. For those on Unix systems the following commands can be used: 
  
```
To find where the file is located use:

  find /path_to_my_src -name foo.f 
  
Once the file is located use: 

  cp /foo.f /path_to_my_src/foo.f 
```

* Now users may follow the instructions provided in the [Users Guide](https://github.com/USEPA/CMAQ/blob/main/DOCS/Users_Guide/CMAQ_UG_ch05_running_a_simulation.md#56-compiling-cmaq-chemistry-transport-model-cctm) to build a CMAQ Project.


## F.3 Option 2: Importing bugfix to your local CMAQ project

* To import the bugfix to your local CMAQ_Project directory, please note the following before following the steps below: 

  - This method requires that you update the file in your BLD directory within scripts directory in your CMAQ project directory, which sits outside of the CMAQ repo. Following the change you will need to re-compile the model. Because this bugfix only updates your local CMAQ project, this method is not recommended by the CMAQ Team asany subsequent projects in other directories must follow the same procedure. 

  - When porting the bugfixed file over to your local project directory, please note any local changes you have made to the pre bugfixed file will be over written by the bugfixed file. Ex. If a specific change is made to the chemistry solver source code (e.g. hrsolver.F), and you are porting in the new code that contains a bugfix in (e.g. hrsolver.F) the specific changes made will be overridden. 
 
  - If you did not build the executable, (e.g. if your system admin built the executable for you) please notify your system admin to rebuild your executable with either option.


* Steps to porting bugfixs to your local project directory:

  - Go to the [CMAQ Issues page](https://github.com/USEPA/CMAQ/issues) and click on your issue.
  
  - Identify which files require updating by looking at the solution, a link to the file(s) should be posted. Click on the link.
  
  - You should now be in the specific issue page, under the Known_Issues folder. Here you should be able to see all the files that need to be updated.
  
  - Download these files to your system storing them in a scratch folder, for those on Unix Systems the following commands can be used:

```
wget https://raw.githubusercontent.com/USEPA/CMAQ/main/DOCS/Known_Issues/CMAQv5.3.1-i#/your_files .
```
  - Once you have the bugfixed files, go to your scripts directory where your BLD directory is created. 
  
  - Within your scripts directory, go to your BLD directory where. Here is where your source code exists for your local project and where your executable lives. 
  
  - Copy the bugfixed files to this folder, effectively replacing the bugged files. 
  
  - Re-compile the model, by typing: make  

* If you are able to unable to re-compile without errors, please contact the author who posted the issue on the [CMAQ Issues page](https://github.com/USEPA/CMAQ/issues).

<!-- BEGIN COMMENT -->

[<< Previous Appendix](CMAQ_UG_appendixE_configuring_WRF.md) - [Home](../README.md) <br>
CMAQ User's Guide (c) 2020<br>
<!-- END COMMENT -->
