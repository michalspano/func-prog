#!/bin/sh

# Run the jekyll server locally in development mode;
# that is, changes are reflected immediately in the browser.

bundle exec jekyll serve --config "_config.yml" --future --livereload
