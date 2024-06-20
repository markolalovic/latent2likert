#!/usr/bin/python3
# -*- coding: utf-8 -*-

''' 
After running 

pkgdown::build_site()

Script:

- replaces "reference/figures" with "../reference/figures" in vignette
- adds `/docs/.nojekyll` for githubpages.
- adds meta description to index.html DONE with pkgdown new version

'''

import os
import sys
import fileinput

def replace(str_wrong, str_right, pathfilename):
    ''' Replaces all occurrences of str_wrong with str_right in pathfilename. '''
    for i, line in enumerate(fileinput.input(pathfilename, inplace=1)):
        sys.stdout.write(line.replace(str_wrong, str_right))

if __name__ == '__main__':
    replace('"reference/figures', '"../reference/figures', './docs/articles/using_latent2likert.html')
    os.system("touch ./docs/.nojekyll")
    print("Done.")