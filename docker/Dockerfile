#Download base image haskell 8.6
FROM haskell:8.6

RUN apt-get update

# Install text editor
RUN apt-get install -y vim less

# Install numactl
RUN apt-get install -y numactl

# Install python
RUN apt-get install -y python
RUN apt-get install -y python3-matplotlib

# Welcome message
RUN echo '[ ! -z "$TERM" -a -r /etc/welcome ] && cat /etc/welcome' \
    >> /etc/bash.bashrc \
    ; echo "\
===================================================================\n\
= Artifact: Compiling First-Order Functions to Session-Typed      =\n\
=                        Parallel Code                            =\n\
===================================================================\n\
\n\
This is the artifact for the paper 'Compiling First-Order \n\
Functions to Session-Typed Parallel Code' .\n\
\n\
PWD=/home/cc20-artifact/session-arr \n\
\n\
This directory contains the (compiled) source code for our tool, \n\
as well as the benchmarks used in out paper, a script for running  \n\
all the examples, and the benchmark data that we obtained. Please \n\
read README.md for a more detailed description. \n\
\n\
  * README.md  ............. Description of this artifact \n\
  * LICENSE  ............... BSD-3 License for the tool \n\
  * benchmark.sh  .......... Re-run all benchmarks \n\
  * src  ................... Source code of the libraries \n\
  * app  ................... Source code of the compiler \n\
  * examples  .............. Source code of the benchmarks \n\
  * benchmark_data  ........ Data used in the paper \n\
\n"\
    > /etc/welcome

# Add cc20-artifact user
RUN useradd -ms /bin/bash cc20-artifact
USER cc20-artifact
WORKDIR /home/cc20-artifact
ENV PATH $HOME/.local/bin:$PATH

# Vim Pathogen
RUN mkdir -p ~/.vim/autoload ~/.vim/bundle && \
    curl -LSso ~/.vim/autoload/pathogen.vim https://tpo.pe/pathogen.vim

# haskell-vim
RUN cd ~/.vim/bundle
RUN git clone https://github.com/neovimhaskell/haskell-vim.git
RUN echo 'syntax on' >> ~/.vimrc
RUN echo 'filetype plugin indent on' >> ~/.vimrc

# Clone artifact
RUN git clone https://github.com/session-arr/session-arr.git session-arr
WORKDIR /home/cc20-artifact/session-arr
RUN stack build

ENTRYPOINT ["/bin/bash"]
