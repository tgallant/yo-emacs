##Introduction

`yo.el` is an Emacs library that caters to a modern frontend workflow.
Interface with yeoman, grunt, bower, and npm right inside of Emacs!

##Setup

`eyo` depends on yeoman, grunt, bower, and npm. Follow your distro's
preferred method for installation.

Debian/Ubuntu:

    sudo apt-get install npm
    sudo npm install -g yo

FreeBSD:

    sudo portmaster www/npm
    sudo npm install -g yo

##Example Usage

Here is an example workflow for an AngularJS project

Create new project with yeoman:

    M-x yo-new <RET> /path/to/proj/ <RET> angular <RET>

Start development server (current buffer must be file from project):

    M-x grunt-server

Add a new controller:

    M-x yo <RET> angular:controller test <RET>

Install new bower dependency to project:

    M-x bower-install-save <RET> your_pkg_here <RET>

Install new npm dependency to project:

    M-x npm-install-save <RET> your_pkg_here <RET>

##Default Keybindings

Keybind   | Command
--------  | --------
`M-g s`   |  grunt-server
`M-g b i` |  bower-install
`M-g b s` |  bower-install-save
`M-g i i` |  npm-install
`M-g i s` |  npm-install-save
`M-g i g` |  npm-install-g
