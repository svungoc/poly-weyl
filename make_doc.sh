#!/bin/bash -ve

dir="math"
#module="Math"

cd /home/san/prog/ocaml/$dir/dune-version
dune build @doc
rsync -avz --delete _build/default/_doc/_html/ docs
for file in $(find docs/ -name index.html)	    
do
    #  sed -i "s|\"../../|\"|g" $file

    # UTF8 right arrow
    sed -i "s|<span>&#45;&gt;</span>|<span class=\"arrow\">→</span>|g" $file

    # we add KaTeX
    sed -i "s|</head>|<link rel=\"stylesheet\" href=\"https://cdn.jsdelivr.net/npm/katex@0.11.1/dist/katex.min.css\" integrity=\"sha384-zB1R0rpPzHqg7Kpt0Aljp8JPLqbXI3bhnPWROx27a9N0Ll6ZP/+DiW/UqRcLbRjq\" crossorigin=\"anonymous\"><script defer src=\"https://cdn.jsdelivr.net/npm/katex@0.11.1/dist/katex.min.js\" integrity=\"sha384-y23I5Q6l+B6vatafAwxRu/0oK/79VlbSz7Q9aiSZUvyWYIYsd+qj+o24G5ZU2zJz\" crossorigin=\"anonymous\"></script><script defer src=\"https://cdn.jsdelivr.net/npm/katex@0.11.1/dist/contrib/auto-render.min.js\" integrity=\"sha384-kWPLUVMOks5AQFrykwIup5lo0m3iMkkHrD0uJ4H5cjeGihAutqP0yW0J6dpFiVkI\" crossorigin=\"anonymous\" onload=\"renderMathInElement(document.body);\"></script></head>|g" $file
done

#sed -i "s| ($package.$module)||g" docs/index.html
#cp ./_build/default/_doc/_html/odoc.css docs/
#cp _build/default/_doc/_html/highlight.pack.js docs/
echo "header nav {display: none;} header nav.toc {display: block;} header dl dd, header dl dt {display: inline-block;} " >>  docs/odoc.css

echo "Done"
