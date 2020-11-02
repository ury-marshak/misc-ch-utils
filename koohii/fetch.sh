#!/bin/sh


curl $1 -o $2 \
  -H 'authority: kanji.koohii.com' \
  -H 'pragma: no-cache' \
  -H 'cache-control: no-cache' \
  -H 'dnt: 1' \
  -H 'upgrade-insecure-requests: 1' \
  -H 'user-agent: Mozilla/5.0 (Macintosh; Intel Mac OS X 10_15_6) AppleWebKit/537.36 (KHTML, like Gecko) Chrome/86.0.4240.111 Safari/537.36' \
  -H 'accept: text/html,application/xhtml+xml,application/xml;q=0.9,image/avif,image/webp,image/apng,*/*;q=0.8,application/signed-exchange;v=b3;q=0.9' \
  -H 'sec-fetch-site: none' \
  -H 'sec-fetch-mode: navigate' \
  -H 'sec-fetch-user: ?1' \
  -H 'sec-fetch-dest: document' \
  -H 'accept-language: en-US,en;q=0.9,he;q=0.8,ru;q=0.7' \
  -H 'cookie: RevTK=YToyOntpOjA7czo2OiJ6b25pdXMiO2k6MTtzOjQwOiIwMzMzZjNlZTgwOGYxZWMwYTk4N2IwM2ZkYjNiNDBlOGRiNTI0NjdjIjt9; koohii=f40nkc31pf4ehknho1fo4qfoo0' \
  --compressed
