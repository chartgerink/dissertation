for doi in `cat dois`;
do
  x=`curl -LH "Accept: text/bibliography; style=bibtex" https://doi.org/$doi`
  if [[ $x =~ @article ]];
  then
    echo $x >> append.bib
  fi
done
