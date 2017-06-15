---
title: Hexo + GitHub Pages搭建个人网页
date: 2017-04-13 15:48:05
tags: Hexo
category: Git
---
### 简介
&emsp;&emsp;本文简单介绍了在Linux平台下进行Hexo + GitHub Pages搭建个人网页的步骤，主要是根据官方文档来一步步实现最基本的功能(本地写文章和预览 + 部署到GitHub供网络访问)

### 参考资料
1 Hexo：[Hexo官网](https://hexo.io)
2 GitHub Pages：[GitHub Pages官网](https://pages.github.com/)
3 Hexo-Theme-Next：[Hexo主题NexT](http://theme-next.iissnan.com/)
<!-- more -->

### Hexo安装
Hexo是快速、简洁且高效的博客框架，使用Node.js语言实现，可以快速解释用Markdown语言写的文章。
安装前提：
Node.js
Git

#### 安装Node.js
安装 Node.js 的最佳方式是使用 nvm。
cURL方式:
``` bash
$ curl https://raw.github.com/creationix/nvm/master/install.sh | sh
```
Wget方式:
``` bash
$ wget -qO- https://raw.github.com/creationix/nvm/master/install.sh | sh
```
安装完成后，重启终端并执行下列命令即可安装 Node.js。
``` bash
$ nvm install stable
```
或者到[Node.js官网](https://nodejs.org/)下载

#### 安装Git
Debian/Ubuntu
``` bash
$ apt-get install git
```
Fedora
``` bash
$ yum install git (up to Fedora 21)
$ dnf install git (Fedora 22 and later)
```
或者到[Git官网](https://git-scm.com/downloads)下载

#### 安装Hexo
所有必备的应用程序安装完成后，即可使用 npm 安装 Hexo。
``` bash
$ npm install -g hexo-cli
```

### Hexo使用
安装完Hexo后，就可以通过Hexo解释Markdown来生成本地静态网页了。
1：首先通过以下命令来初始化目录：
``` bash
$ hexo init <folder> // folder为Hexo主目录文件夹名
```
新建完成后，指定文件夹的目录如下：
.
├── _config.yml // Hexo配置文件
├── node_module // Node.js环境
├── package.json // 应用程序的信息
├── scaffolds // 模板
├── source // 资源
|   ├── _drafts // 草稿
|   └── _posts // 网页文章
└── themes // 主题

2：然后进入到`<folder>`，通过以下命令来生成静态网页
``` bash
$ cd <folder>
$ hexo generate // 或者简写：hexo g
```
这时在目录下多出了个public的文件夹，这个文件夹就是生成的静态网页存放的位置

3：最后通过以下命令来启动本地服务器，然后访问`http://localhost:4000/`来预览网页
``` bash
$ hexo serve // 或者简写：hexo s
```

4：写作，通过以下命令来创建新的文章
``` bash
$ hexo new "NewPost" // NewPost为文章名
```
或者直接在source/_post目录编辑新的Markdown文件

### Hexo配置
Hexo配置文件为主目录下的_config.yml，可以通过修改基中的配置项来配置个人网页。
title：网站标题
subtitle：网站副标题
description：网站描述
author：您的名字
language：网站使用的语言
timezone：网站时区。Hexo 默认使用您电脑的时区。时区列表。比如说：America/New_York,Japan,和UTC。
source_dir：资源文件夹，这个文件夹用来存放内容。默认为：source
public_dir：公共文件夹，这个文件夹用于存放生成的站点文件。默认为：public
tag_dir：标签文件。默认为：tags
archive_dir：归档文件。默认为：archives
category_dir：分类文件。默认为：categories
code_dir：Include code 文件。默认为：downloads/code
i18n_dir：国际化（i18n）文件。默认为:lang
skip_render : 跳过指定文件的渲染，您可使用 glob 表达式来匹配路径。
theme：当前主题名称。值为false时禁用主题
deploy：部署网络服务器的配置。
详细配置可浏览_config.yml文件或者官方文档。

### Hexo主题
Hexo提供了丰富的主题，访问[这里](https://hexo.io/themes/)可获得更多的主题。安装完Hexo后，默认的主题是landscape，存放在主目录的themes文件夹下。
使用NexT主题：
1 进入到主题目录 
``` bash
$ cd themes
```
2 从GitHub克隆主题到本地
``` bash
$ git clone https://github.com/iissnan/hexo-theme-next
```
3 修改_config.yml的theme配置为:hexo-theme-next
同时NexT主题也可以根据个人喜好进行配置，具体方法参考[NexT官网](http://theme-next.iissnan.com/)

以上部分，完成了Hexo安装，同时知道怎么使用Hexo来生成和预览本地的静态网页，那么接下来就是怎么把本地的网页部署到网络上。

### GitHub Pages
GitHub Pages是指托管在GitHub上的个人网页或者项目网页。这里使用的是个人网页，也就是把Hexo生成的静态网页部署到GitHub上，然后通过username.git.io来访问这个网页。
1 要使用GitHub，首先是注册一个GitHub账号。访问[这里](https://github.com/join)来进行GitHub账号注册。
2 注册完后，登录GitHub，创建名为username.git.io的仓库（repository）。其中username为GitHub账号名。
3 配置Hexo，修改_config.yml的deploy配置，如下：
deploy:
&emsp;type: git // 使用GitHub作为网页服务器
&emsp;repo: `https://github.com/username/username.git.io.git` // 部署到username.git.io的GitHub仓库
&emsp;branch: master // 仓库的master分支
&emsp;message: "update website" // 部署提交的日志信息
4 在部署到GitHub前，需要安装Hexo的git部署工具，进入到Hexo主目录，通过以下命令安装：
``` bash
$ npm install hexo-deployer-git --save
```
5 通过以下命令来部署网页：
```bash
$ hexo deploy // 或者简写：hexo d
```
6 部署完成后，通过username.git.io就可以访问网页了。

### 总结
至此，使用Hexo解释Markdown文件来生成和预览本地静态网页，到部署到GitHub的步骤就完成了。虽然这里只是简单介绍了Hexo的基本功能，但其却不仅限于此。得益于丰富的插件（Plugins），可以更加完善个人网页的功能（比如评论），至于能达到什么样的程度，就由各位看官们自己去挖掘和扩展了。
