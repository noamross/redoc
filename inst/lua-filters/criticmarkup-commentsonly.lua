local commentholder = {}
return {
  {
    Span = function (elem)
      local returnval
      if elem.classes[1] and elem.classes[1] == "insertion" then
        returnval = elem.content
      elseif elem.classes[1] and elem.classes[1] == "deletion" then
        returnval = {}
      elseif elem.classes[1] and elem.classes[1] == "comment-start" then
        commentholder[elem.attributes["id"]] = elem
        returnval = pandoc.RawInline(FORMAT, "{== ")
      elseif elem.classes[1] and elem.classes[1] == "comment-end" then
        local comment = commentholder[elem.attributes["id"]]
        if comment == nil then
          returnval = pandoc.RawInline(FORMAT, "")
        elseif #comment.content == 0 then
          returnval = pandoc.RawInline(FORMAT, " ==}")
        else
          returnval = { pandoc.RawInline(FORMAT, " ==}{>> ") } ..
            comment.content ..
            { pandoc.RawInline(FORMAT, " <<}") }
        end
      else
        returnval = nil
      end
    return returnval
    end
  },
}
