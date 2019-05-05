-- A Pandoc filter for reversing knitted documents.  Text generated from
-- code in R Markdown has custom styles with names of the chunks that generated
-- them.  The filter replaces Divs and Spans with these custom styles with
-- placeholders like <<<redoc-inline-1>>, to be replaced with chunk content
-- later

function Div(elem)
  if elem.attributes["custom-style"] and string.find(elem.attributes["custom-style"], "redoc%-") then
    return pandoc.Para(pandoc.RawInline(FORMAT, "<<<"..elem.attributes["custom-style"]..">>>"))
  elseif elem.attributes["custom-style"] then
     return elem.content
  else
    return nil
  end
end

function Span(elem)
  if elem.classes[1] and elem.classes[1] == "anchor" then
    return {}
  elseif elem.attributes["custom-style"] and string.find(elem.attributes["custom-style"], "redoc%-") then
    return pandoc.RawInline(FORMAT, "<<<"..elem.attributes["custom-style"]..">>>")
  elseif elem.attributes["custom-style"] then
    return elem.content
  else
    return nil
  end
end
