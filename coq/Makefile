files=ch0.v  ch1.v  ch2.v  ch3.v  ch4.v  ch6.v  ch7.v \
ch7.1-n--tuples,-lists-with-an-invariant-on-the-length.v \
ch7.2-n--tuples,-a-sub-type-of-sequences.v \
ch7.3-Finite-types-and-their-theory.v \
ch7.4-The-ordinal-subtype.v ch7.5-Finite-functions.v ch7.6-Finite-sets.v \
ch7.7-Permutations.v

all: $(files:%.v=%.html) index.html

ch%.html: ch%.v
	cat jscoq_header.html > $@
	echo -n "<h1>Chapter " >> $@
	echo -n $* | sed "s/--/@@/g" | sed "s/-/ | /" | sed "s/-/ /g" | sed "s/@@/-/g" >> $@
	echo "</h1><div><textarea id='coq-code'>" >> $@
	cat $< >> $@
	echo >> $@
	echo "</textarea></div>" >> $@
	echo "<script type='text/javascript'>" >> $@
	echo "var coqdoc_ids = ['coq'];" >> $@
	echo "</script>" >> $@
	cat jscoq_footer.html >> $@

index.html:
	cat index_header.html > $@
	for file in $(files) ; do \
		echo -n "<a href=\"" >> $@ ; \
		echo -n $$file | sed "s/\.v/\.html/" >> $@ ; \
		echo -n "\">" >> $@ ; \
                echo -n "Chapter " >> $@ ; \
                echo -n $$file | sed "s/\.v//" | sed "s/^ch//" | sed "s/--/@@/g" | \
			sed "s/-/ /g" | sed "s/@@/-/g" >> $@ ; \
                echo "</a><br> " >> $@ ; \
	done
	cat index_footer.html >> $@

clean:
	rm -f $(files:%.v=%.html) index.html
