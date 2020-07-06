#!/bin/bash

# GO111MODULE=on
go get golang.org/x/tools/gopls@latest

sudo apt-get install clang-tools-8
sudo update-alternatives --install /usr/bin/clangd clangd /usr/bin/clangd-8 100

# Clangd tries to locate the “compile_commands.json” file in the root
# of the project, so it’s useful to make a symlink in the project
# root and to where it’s located in a build folder. Most build tools
# can output “compile_commands.json”. In CMake you write:
#    set(CMAKE_EXPORT_COMPILE_COMMANDS ON)

# sudo pacman -S the_silver_searcher
