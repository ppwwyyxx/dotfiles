local template = {
    g = "http://www.google.com/search?q=%s",
    gfl =  "http://google.com/search?q=%s&btnI=I%%27m+Feeling+Lucky",
    gi = "http://images.google.com/images?q=%s&&tbo=1",
    gm = "http://maps.google.com/maps?q=%s",

    py = "http://www.google.com/search?q=%s&domains=docs.python.org&sitesearch=docs.python.org&sourceid=google-search&submit=submit",
    pym = "http://docs.python.org/lib/module-%s.html",
    pypi = "http://pypi.python.org/pypi?%%3Aaction=search&term=%s&submit=search",

    ruby = "http://google.com/search?q=site:ruby-doc.org+%s&btnI=I%%27m+Feeling+Lucky",
    cpp = "http://en.cppreference.com/mwiki/index.php?title=Special%%3ASearch&search=%s&button=",
    github = "https://github.com/search?q=%s",

    wp =  "http://en.wikipedia.org/?search=%s",
    dbm = "http://movie.douban.com/subject_search?search_text=%s&cat=1002",
    dbb = "http://book.douban.com/subject_search?search_text=%s&cat=1001",
    mp3 = "http://music.baidu.com/search?key=%s",
    etym = "http://www.etymonline.com/index.php?allowed_in_frame=0&search=%s&searchmode=none"
}

function web_cmd(cmd)
    local space = cmd:find(" ")
    if space then
        local web = cmd:sub(0, space - 1)
        local arg = cmd:sub(space + 1)
        local t = template[web]
        if t then
            return t:format(arg)
        end
    end
    return template['g']:format(cmd)
end
