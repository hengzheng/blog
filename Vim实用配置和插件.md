---
title: Vim实用配置和插件
date: 2017-04-14 11:42:31
tags: Vim
category: Vim
---
### 简介
&emsp;&emsp;本文简单介绍Vim的一些实用配置，比如修改了Vim配置即刻生效，打开文件时定位到上次关闭的位置等，同时介绍一些比较实用的插件及其配置。

### 基础配置
```
" get out of horrible vi-compatible mode
set nocompatible 
```
compatible/nocompatible:This option has the effect of making Vim either more Vi-compatible, or
make Vim behave in a more useful way.
从说明中可以看出这样设置可以使Vim更好用，而且这里有些配置Vi是不支持的，所以关闭Vi兼容。
<!-- more -->

```
filetype plugin indent on
```
检测文件类型，如果一个文件在编辑，其对应类型的插件和缩进自动加载。Vim插件依赖这个配置。

```
syntax on " 语法高亮，很多配置依赖此设置

"set tab
set expandtab " 空格代替tab
set tabstop=4 " 代替tab的空格数量
set shiftwidth=4 " 缩进的空格数
set smarttab " 位于行首的tab代替成的空格用shiftwidth定义的数量(因为tabstop不一定跟shiftwidth相等)

set number " 显示行号

" Set to auto read when a file is changed from the outside
set autoread " 当前文件在Vim外被编辑且当前Vim内未修改，则自动加载

" Set backgroud
set background=dark " 设置背景颜色
colorscheme vividchalk " 设置主题

" search setting
set ignorecase " 搜索时忽略大小写
set smartcase  " 如果搜索文本有大写，ignorecase则失效
set nowrapscan " 搜索到头或到尾就结束，不循环
set incsearch " 当输入搜索文本时，在文件中高亮匹配项
set hlsearch " 在文件中高亮前一个搜索的文本匹配项
```

### 实用配置
#### .vimrc修改保存时自动加载
``` code
" reload vimrc
if has("autocmd")
    autocmd! bufwritepost $HOME/.vimrc source $HOME/.vimrc
endif
```
其实命令很简单，如果vim当前buf保存的是.vimrc文件，则执行source .vimrc。相当于在修改.vimrc保存后，手动执行:source %（表示当前文件）。

#### 打开文件时定位到上次关闭的位置
``` code
" back to the position where last quit
if has("autocmd")
    autocmd! bufreadpost * if line("'\"") > 1 && line("'\"") <= line("$") | exe "normal! g`\"" | endif
endif
```
line('")获取上次退出时所在的行号，line($)获取最大行号。当打开文件时，如果上次退出时所在行大于1或者小于等于最后一行，则跳到那行。

### 插件
#### Vundle插件管理器
GitHub地址：[https://github.com/VundleVim/Vundle.vim](https://github.com/VundleVim/Vundle.vim)
安装：
Linux环境下命令，且需要用到Git，搜索需要用到Curl
```
$ git clone https://github.com/VundleVim/Vundle.vim.git ~/.vim/bundle/Vundle.vim
```
插件管理途径：
.vimrc文件添加需要的插件
Plugin 'PluginName' ([http://vim-scripts.org/vim/scripts.html](http://vim-scripts.org/vim/scripts.html)下的插件)
Plugin 'Author/PluginName' (GitHub仓库的插件)
Plugin 'PluginAddr' (非GitHub托管的插件)
Plugin 'file://path_to_local_plugin' (本地插件)
插件管理命令：
安装插件：
:PluginInstall [PluginName] // 安装对应插件，不填则全部安装
搜索插件：
:PluginSearch PluginName // 搜索插件
删除插件：
:PluginClean [PluginName] // 删除~/.vim/bundle目录下不在配置中的插件
更新插件：
:PluginUpade [PluginName] // 更新对应插件，不填则全部更新

#### minibufexpl
github地址：[https://github.com/fholgado/minibufexpl.vim](https://github.com/fholgado/minibufexpl.vim)
在Vim编辑时像浏览器网页标签一样显示所有打开的buf。
![](minibufexpl.gif)

#### airline
GitHub地址：[https://github.com/vim-airline/vim-airline](https://github.com/vim-airline/vim-airline)
美观的状态栏，同时可以显示更多的信息。
![](airline.gif)

#### leaderf
GitHub地址：[https://github.com/vim-scripts/LeaderF](https://github.com/vim-scripts/LeaderF)
文件查找，需要Vim编译时支持Python
![](leaderf.gif)

### 总结
Vim是一款强大的编辑器，各位看官们可以根据其丰富的配置和插件，打造出完全属于自己的IDE。
