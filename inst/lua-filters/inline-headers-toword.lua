-- This filter converts headers below a fixed level to inline headers in word
-- Metadata should have a value `inlineheaders:`.

local inline_header_lev

function get_lev (meta)
    inline_header_lev = tonumber(meta.inlineheaders)
end

local last_headspan

replace_heds = {
  Header = function(el)
    if inline_header_lev and el.level >= inline_header_lev then
      headspan = pandoc.Span(el.content .. {pandoc.Str(": ")})
      headspan.attributes["custom-style"] = "Heading" .. tostring(el.level) .. "Char"
      last_headspan = headspan
      return {}
    else
      return nil
    end
  end,

   Para = function(el)
    if last_headspan then
      el.content = {last_headspan} .. el.content
      last_headpan = nil
      return el
    else
      return nil
    end
  end
}


return {{Meta = get_lev}, replace_heds}

