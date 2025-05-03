# emacs-websearch
Use request.el to search things on web

This is a simple package which let you search anything on web using emacs. It can be used with Vertico! Currently, it only supports google. other search engine would be added soon.

The default brower can be changed by modifying `browse-url-browser-function`, e.g.

- `(setq browse-url-browser-function 'xwidget-webkit-browse-url)`
- `(setq browse-url-browser-function 'eaf-open-browser)`

When there is no mark set, the default search term is set to `thing-at-point`. When envoked on an active region, the default serach term is set to the region's content.

## install

```
(use-package emacs-websearch
  :straight '(emacs-websearch :host github :repo "zhenhua-wang/emacs-websearch")
  :bind (("C-c l" . emacs-websearch)))
```

Asynchronously search is available for Consult/Vertico users. To use this, `(setq emacs-websearch-async t)`

Currently, DuckDuckGo and Google are supported, and the default search engine is DuckDuckGo. This can be changed by `(setq emacs-websearch-engine 'google)`

*Remote* TTY users are recommended to use DuckDuckGo with `(setq browse-url-browser-function 'eww-browse-url)`

## example

![example](img/example.png)
