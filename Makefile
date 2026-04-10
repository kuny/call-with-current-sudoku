#--------------------------------------------------
# Project:
# Purpose:
#--------------------------------------------------

.PHONY: clean build run debug test

all: clean build

clean:
	@rm ./main

build:
	@raco exe main.rkt

run:
	@rlwrap ./main

debug:
	@rlwrap racket main.rkt

test:
	@raco test main.rkt

