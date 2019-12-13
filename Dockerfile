#Download base image haskell 8.6
FROM haskell:8.6

###############################################################################
# Install dependencies

RUN apt-get update && apt-get install -y \
  vim \
  less \
  numactl \
  python \
  python-matplotlib \
  python-pint

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

###############################################################################
# Artifact user

# Add cc20-artifact user
RUN useradd -ms /bin/bash cc20-artifact

###############################################################################
# Download and build artifact

USER cc20-artifact

# Vim Pathogen
RUN mkdir -p /home/cc20-artifact/.vim/autoload ~/.vim/bundle && \
    curl -LSso /home/cc20-artifact/.vim/autoload/pathogen.vim https://tpo.pe/pathogen.vim && \
    echo 'execute pathogen#infect()' >> /home/cc20-artifact/.vimrc && \
    echo 'syntax on' >> /home/cc20-artifact/.vimrc && \
    echo 'filetype plugin indent on' >> /home/cc20-artifact/.vimrc && \
    echo 'colorscheme default' >> /home/cc20-artifact/.vimrc && \
    git clone https://github.com/neovimhaskell/haskell-vim.git /home/cc20-artifact/.vim/bundle/haskell-vim

# Clone artifact

RUN git clone https://github.com/session-arr/session-arr.git \
        /home/cc20-artifact/session-arr && \
    echo 'system-ghc: true' >> /home/cc20-artifact/session-arr/stack.yaml

WORKDIR /home/cc20-artifact/session-arr
RUN stack build
ENV TERM xterm-256color

CMD ["/bin/bash", "-l"]
