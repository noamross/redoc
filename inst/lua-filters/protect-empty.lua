-- This filter takes empty divs and spans that have custom style attributes,
-- which would be removed in a word doc, and replaces them with raw openxml
-- of empty hidden paragraphs and single-symbol hidden text runs, which
-- should survive round-tripping.

function Div(elem)
  if elem.classes[1] == "redoc" then
    elem.attributes["custom-style"] = elem.identifier
    if #elem.content == 0 or
       (#elem.content == 1 and
         (elem.content[1].tag == "RawBlock" or elem.content[1].tag == "RawInline")
       ) then
      elem.content = {
        pandoc.RawBlock(
          "openxml",
          '<w:p><w:pPr><w:pStyle w:val="' .. elem.identifier .. '"/>' ..
          '<w:rPr><w:vanish/></w:rPr></w:pPr><w:r><w:rPr><w:vanish/></w:rPr>' ..
           '<w:t xml:space="preserve"> </w:t></w:r></w:p>'
      )}
    end
    return elem
  else
    return nil
  end
end

function Span(elem)
  if elem.classes[1] == "redoc" then
    elem.attributes["custom-style"] = elem.identifier
    if #elem.content == 0 or
       (#elem.content == 1 and
         (elem.content[1].tag == "RawBlock" or elem.content[1].tag == "RawInline")
       ) then
    elem.content = {
      pandoc.RawInline("openxml",
      '<w:r><w:rPr><w:rStyle w:val="' .. elem.identifier .. '"/>' ..
      '<w:vanish/></w:rPr><w:t xml:space="preserve"> </w:t></w:r>'
      )}
    end
    return elem
  else
    return nil
  end
end
