#--------------------------------------------------
# Project:
# Purpose:
#--------------------------------------------------

.PHONY: clean build exe run test

all: clean build

clean:
	@rm ./main

build:
	@raco exe main.rkt

exe:
	@rlwrap ./main

run:
	@rlwrap racket main.rkt

test:
	@raco test main.rkt

