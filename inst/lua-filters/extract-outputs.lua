-- A Pandoc filter for reversing knitted documents.  Text generated from
-- code in R Markdown has custom styles with names of the chunks that generated
-- them.  This filter extracts the content of those sections

local contentlist = pandoc.Meta({})

function CodeBlock(elem)
  elem.classes[1] = "codeblock"
  return(elem)
end

function Div(elem)
  if elem.attributes["custom-style"] and string.find(elem.attributes["custom-style"], "redoc%-") then
    if contentlist[elem.attributes["custom-style"]] then
      contentlist[elem.attributes["custom-style"]] =
        pandoc.Div(contentlist[elem.attributes["custom-style"]].content ..  elem.content)
    else
      contentlist[elem.attributes["custom-style"]] =
        pandoc.Div(elem.content)
    end
  else
    return nil
  end
end

function Span(elem)
  if elem.attributes["custom-style"] and string.find(elem.attributes["custom-style"], "redoc%-") then
    if contentlist[elem.attributes["custom-style"]] then
      contentlist[elem.attributes["custom-style"]] =
        pandoc.Div(contentlist[elem.attributes["custom-style"]].content .. {pandoc.Para(elem.content)})
    else
      contentlist[elem.attributes["custom-style"]] =
        pandoc.Div(pandoc.Para(elem.content))
    end
  else
    return nil
  end
end

function Doc (blocks, meta)
  local mb = pandoc.Meta({})
  for k,v in pairs(contentlist) do
    mb[k] = pandoc.MetaBlocks(v.content)
  end
  return pandoc.Doc({}, mb)
end
