-- This filter finds inline headers in word and makes them full headers

Para = function(el)
  if el.content[1].attributes then
    _, _, lev = string.find(el.content[1].attributes["custom-style"], "Heading(%d)Char")
    if lev then
      pre_hed = pandoc.Header(tonumber(lev), el.content[1].content)
      table.remove(el.content, 1)
      return {pre_hed, el}
    else
      return nil
    end
  else
    return nil
  end
end


dropstyles = function(el)
  if el.attributes then
    if el.attributes["custom-style"] then
      return el.content
    else
      return nil
    end
  else
    return nil
  end
end

return {{Para = Para}, {Inline = dropstyles, Block = dropstyles}}

