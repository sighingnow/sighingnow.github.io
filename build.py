#!/usr/bin/env python3
# -*- coding: utf-8 -*-

import os

def build_pages(src, dst):
    for f in os.listdir(src):
        srcpath, dstpath = '%s/%s'%(src, f), '%s/%s'%(dst, f)
        if os.path.isdir(srcpath):
            build_pages(srcpath, dstpath)
        elif f.endswith('.md'):
            if not os.path.exists(dst):
                os.makedirs(dst)
            print('build %s'%(srcpath))
            os.system('pandoc %s -o %s --mathjax --highlight-style=pygments -S'%(srcpath, dstpath[:-3]+'.html'))
            meta, text = '', ''
            with open(srcpath, 'r', encoding='utf-8') as fp:
                meta = '\n'.join(fp.read().split('\n')[0:9])+'\n'
            with open(dstpath[:-3]+'.html', 'r', encoding='utf-8') as fp:
                text = fp.read()
            with open(dstpath[:-3]+'.html', 'w', encoding='utf-8') as fp:
                fp.write(meta)
                fp.write(text)

if __name__ == '__main__':
    build_pages('_pages', '_posts')
