#--------------------------------------------------
# Project:
# Purpose:
#--------------------------------------------------

.PHONY: run test

run:
	@chez --script src/main.ss

test:
	@chez --script test/test.ss

