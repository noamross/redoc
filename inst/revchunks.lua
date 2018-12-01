-- A Pandoc filter for reversing knitted documents.  Text generated from
-- code in R Markdown has custom styles with names of the chunks that generated
-- them.  The filter replaces Divs and Spans with these custom styles with
-- placeholders like [[inline-1]] and [[chunk-setup]], to be replaced with
-- chunk content lateer

function Div(elem)
  if elem.attributes["custom-style"] then
    local i = string.find(elem.attributes["custom-style"], "chunk%-")
    if i == 1 then
      i = 0
      return pandoc.Para(pandoc.RawInline(FORMAT, "[["..elem.attributes["custom-style"].."]]"))
    else
      return elem.content
    end
  else
    return elem
  end
end

function Span(elem)
  if elem.attributes["custom-style"] then
    local i = string.find(elem.attributes["custom-style"], "inline%-")
    if i == 1 then
      i = 0
      return pandoc.RawInline(FORMAT, "[["..elem.attributes["custom-style"].."]]")
    else
      return elem.content
    end
  else
    return elem
  end
end
