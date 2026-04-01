#--------------------------------------------------
# Project:
# Purpose:
#--------------------------------------------------

.PHONY: run test

run:
	racket main.rkt

test:
	raco test main.rkt

