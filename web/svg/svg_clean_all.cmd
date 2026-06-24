@echo off
rem --https://github.com/RazrFalcon/svgcleaner-- no it remove color name "black"
rem use https://github.com/scour-project/scour/
cd light
rem for %%f in (*.svg) do svgcleaner "%%f" "%%f" --keep-named-ids

for %%f in (*.svg) do (
    ren %%f %%f.temp
    scour -i %%f.temp %%f --enable-comment-stripping --shorten-ids --indent=none --disable-simplify-colors --strip-xml-prolog --remove-titles --no-line-breaks
    del %%f.temp
)
pause
