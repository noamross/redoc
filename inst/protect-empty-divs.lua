-- This filter takes empty dives that have custom style attributes, which
-- would be removed in a word doc, and replaces them with raw openxml
-- that should survive round-tripping

function Div(elem)
  if elem.attributes["custom-style"] and #elem.content == 0 then
    return pandoc.RawBlock("openxml",
      '<w:p><w:pPr><w:pStyle w:val="' ..
      elem.attr.attributes['custom-style'] ..
      '"/><w:rPr><w:vanish/></w:rPr></w:pPr></w:p>'
    )
  else
    return elem
  end
end
