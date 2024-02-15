#!/usr/bin/env bash

docker run -it -v "$(pwd)":"$(pwd)" mermaid-cli:local $@
