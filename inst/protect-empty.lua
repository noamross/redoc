-- This filter takes empty divs and spans that have custom style attributes,
-- which would be removed in a word doc, and replaces them with raw openxml
-- of empty hidden paragraphs and single-symbol hidden text runs, which
-- should survive round-tripping.

function Div(elem)
  if elem.attributes["custom-style"] and
     string.match(elem.attributes["custom-style"], "^chunk") and
     #elem.content == 0 then
    return pandoc.RawBlock("openxml",
      '<w:p><w:pPr><w:pStyle w:val="' ..
      elem.attr.attributes['custom-style'] ..
      '"/><w:rPr><w:vanish/></w:rPr></w:pPr></w:p>'
    )
  else
    return nil
  end
end

function Span(elem)
  if elem.attributes["custom-style"] and
     string.match(elem.attributes["custom-style"], "^chunk") and
     #elem.content == 0 then
    return pandoc.RawInline("openxml",
      '<w:r><w:rPr><w:rStyle w:val="' ..
      elem.attr.attributes['custom-style'] ..
      '"/><w:vanish/></w:rPr><w:sym w:font="Times New Roman" w:char="F020"/></w:r>'
    )
  else
    return nil
  end
end
