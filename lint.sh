#/bin/sh

for d in app src test; do
    hlint $d;
done

grep -REn "\s+$" \
    -m 10 \
    --color=always \
    --exclude-dir dist-newstyle \
    --exclude-dir .git \
    --exclude "*.wav" \
    --exclude "*.bin"
