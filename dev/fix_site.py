#!/usr/bin/python3
# -*- coding: utf-8 -*-

''' 
Script adds:

- `/docs/.nojekyll` for githubpages.
- meta description to index.html

After running pkgdown::build_site()
'''

import os
import sys
import fileinput

def add_meta():
    filename = './docs/index.html'
    meta_line = '<meta name="description" content="Converting latent variables into Likert scale responses in R. Package latent2likert converts continuous latent variables into ordinal categories to simulate Likert scale item responses." />'

    with open(filename, 'r') as f:
        in_file = f.readlines()

    out_file = []
    for line in in_file:
        out_file.append(line)
        if '</title>' in line:
            out_file.append(meta_line)    
            
    with open(filename, 'w') as f:
        f.writelines(out_file)    

if __name__ == '__main__':
    os.system("touch ./docs/.nojekyll")
    add_meta()
    print("Done.")