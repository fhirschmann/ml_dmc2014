.PHONY: doc data

doc:
	$(MAKE) -C doc

data:
	python bin/add-holiday.py
	Rscript bin/make-data.R
