#!/usr/bin/python3
# -*- coding: utf-8 -*-

''' 
Script adds:

- `/docs/.nojekyll` for githubpages.
- meta description to index.html DONE

After running pkgdown::build_site()
'''

import os

if __name__ == '__main__':
    os.system("touch ./docs/.nojekyll")
    print("Done.")