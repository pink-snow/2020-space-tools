default: out/annotated out/annotated-pf

clean:
	rm -rf out

.DELETE_ON_ERROR:

.PHONY: default clean out/annotated out/annotated-pf

out/annotated: out/annotated.done
out/annotated-pf: out/annotated-pf.done
out/pflockingen: out/pflockingen.done

################################################################################
# Download and unpack the data
################################################################################

out/message-from-space:
	@mkdir -p out
	git clone --depth 1 https://github.com/zaitsev85/message-from-space $@

out/pflockingen.zip:
	@mkdir -p out
	wget -O $@ \
		https://pflockingen.serveblog.net/files/pflockingen_cygnus_images_2020-07-09.zip

out/pflockingen.done: out/pflockingen.zip
	@mkdir -p out/pflockingen
	cd out/pflockingen && unzip ../pflockingen.zip
	touch $@

################################################################################
# Binaries
################################################################################

out/annotate: annotate.hs
	@mkdir -p out
	rm -f out/*.o out/*.hi
	ghc -O -outputdir out -o $@ $<
	rm -f out/*.o out/*.hi

out/annotate-pf: annotate-pf.hs
	@mkdir -p out
	rm -f out/*.o out/*.hi
	ghc -O -outputdir out  -o $@ $<
	rm -f out/*.o out/*.hi

################################################################################
# Annotations
################################################################################

out/annotated.done: out/message-from-space out/annotate $(wildcard out/message-from-space/source/message*.png)
	rm -rf out/annotated && mkdir -p out/annotated
	set -e; for i in {2..42}; do \
		ii=$$(printf "%02d" $$i); \
		./out/annotate \
			out/message-from-space/source/message$$i.png \
			out/annotated/$$ii.svg out/annotated/$$ii.txt; \
		convert \
			out/message-from-space/source/message$$i.png \
			-filter point -resize 200% out/annotated/$$ii.png; \
	done
	cd out/annotated && ../../gen-index.sh > index.html
	touch $@

out/annotated-pf.done: out/pflockingen out/annotate-pf $(wildcard out/pflockingen/signal*.png)
	rm -rf out/annotated-pf && mkdir -p out/annotated-pf
	set -e; for i in {01..15}; do \
		./out/annotate-pf \
			out/pflockingen/signal$$i.png \
			out/annotated-pf/$$i.svg out/annotated-pf/$$i.txt; \
		convert out/pflockingen/signal$$i.png \
			-filter point -resize 800% out/annotated-pf/$$i.png; \
	done
	cd out/annotated-pf && ../../gen-index.sh > index.html
	touch $@
