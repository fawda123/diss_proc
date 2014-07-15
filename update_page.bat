ECHO

REM if new repo, create project page (settings, automatic page generator)
REM add the following to project page for whatever pdfs you want to have viewable
REM [Intro.pdf](http://USERNAME.github.io/REPONAME/Intro.pdf)
REM now you have a gh-pages branch in your repo
REM then do the following to sync project page pdfs in master branch to gh-pages
REM repeat for each new commit/push to main granch

cd C:\Documents\blog\diss_proc

git checkout gh-pages
git pull
git checkout master -- "*.pdf"
git commit -m "copy pdf to gh-pages"
git push
git checkout master


