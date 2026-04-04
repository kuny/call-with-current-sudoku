#--------------------------------------------------
# Project:
# Purpose:
#--------------------------------------------------

.PHONY: run test

run:
	@rlwrap racket main.rkt

test:
	@raco test main.rkt

