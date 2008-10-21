#!/bin/sh

git filter-branch \
    --env-filter 'test "$GIT_AUTHOR_NAME" = johnw -o "$GIT_AUTHOR_NAME" = johnw@newartisans.com -o "$GIT_AUTHOR_NAME" = jwiegley && export GIT_AUTHOR_NAME="John Wiegley" GIT_AUTHOR_EMAIL=johnw@newartisans.com GIT_COMMITTER_NAME="John Wiegley" GIT_COMMITTER_EMAIL=johnw@newartisans.com || cat' \
    --index-filter 'git update-index --remove *.pyc .cvsignore .arch-inventory' \
    --msg-filter "egrep -v '(darcs-hash|\<M\> |r[0-9]*@Hermes:)' | sed 's/\\*\\*\\*DARCS\\*\\*\\*//' | sed 's/empty log message/no comment/'" \
    $1
