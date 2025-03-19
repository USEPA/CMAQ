## This script was created to address the markdown links within the CMAQ User's Guide Documentation
## that have been decided to have different paths within the Github Markdown and SPHINX HTML
## versions of the link.

## Author: Jon Brunton
## Co-Authors: Kristen Foley & Fahim Sidi

import os
import glob

## Method for reading and editing relative link sections of markdown files
def remove_rel_links(path):
    '''
    inputs
        path (string): A single path to a text file with a relative links section, to be deleted
    
    '''
    # open the file and read in the data line-by-line
    with open(path, 'r') as file:
        lines = file.readlines()
    # parse the lines to determine the section of lines
    # used for labeling relative links
    for i in range(len(lines)):
        if 'relative_links_start' in lines[i]:
            start = i
        elif 'hardcode_links' in lines[i]:
            end = i
    try:
        # edit the relative links section data in access memory
        lines[start:end+1] = ' '
        # rewrite the relative links section to be blank
        with open(path, 'w') as file:
            file.writelines(lines)
        # close file
        file.close()
        # print file has been edited
        print('Relative links removed from: ', path)
    # error exception if file has no relative links
    except:
        file.close()

## See FY23-26 Release OneNote for decision details, or contact Kristen/Fahim above.
## INPUT FOLDER PATHS HERE - all markdown that will have HTML built from it in source/
folder_list = ['./source/', './source/DOCS/',
               './source/DOCS/Users_Guide/', './source/DOCS/Release_FAQ/',
               './source/DOCS/Release_Notes/', './source/DOCS/Developers_Guide/',
               './source/DOCS/Users_Guide/Tutorials/',
               './source/DOCS/Users_Guide/Appendix'
              ]
file_list = []
## append all .md files in listed folders to list
for folder in folder_list:
    for file in glob.glob(os.path.join(folder, '*.md')):
        file_list.append(file)

## Iterate through list and make edits
for file in file_list:
    remove_rel_links(file)
