#!/usr/bin/env zsh

function writeTOC() {

cat <<END
> <center><hr style="width:30%;float:left;border-color:#CCCCD0;margin-top:1em"/><span class="sc"><b>Table of Content</b></span><hr style="width:30%;float:right;border-color:#CCCCD0;margin-top:1em"/></center>
> 
> * This will be replaced by the ToC
> {:toc}
>
END

}

cat config.ymd

writeTOC

cat <<END

enddiv

END

for fic in **/*.lhs(.N); do
    contains_haskell=$(( $( egrep '^>' $fic | wc -l) > 0 ))
    ((contains_haskell)) && \
        print -- "\n<hr/><a href=\"code/$fic\" class=\"cut\">Download the source code of this section → ${fic:h}/<strong>${fic:t}</strong></a>\n"
    cat $fic
    ((contains_haskell)) && \
        print -- "\n<a href=\"code/$fic\" class=\"cut\">Download the source code of this section → ${fic:h}/<strong>${fic:t}</strong> </a>\n"
done | perl -pe 'BEGIN{$/="";} s#((^>.*\n)+)#<div class="codehighlight">\n<code class="haskell">\n$1</code>\n</div>\n#mg' | perl -pe 's#^> ?##' | perl -pe 's/^ #/#/'
