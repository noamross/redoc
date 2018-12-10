local commentholder = {}
return {
  {
    Span = function (elem)
      if elem.classes[1] and elem.classes[1] == "insertion" then
        local opener = { pandoc.RawInline(FORMAT, "{++ ") }
        local closer = { pandoc.RawInline(FORMAT, " ++}") }
        return opener .. elem.content .. closer
      elseif elem.classes[1] and elem.classes[1] == "deletion" then
        local opener = { pandoc.RawInline(FORMAT, "{-- ") }
        local closer = { pandoc.RawInline(FORMAT, " --}") }
        return opener .. elem.content .. closer
      elseif elem.classes[1] and elem.classes[1] == "comment-start" then
        commentholder[elem.attributes["id"]] = elem
        return pandoc.RawInline(FORMAT, "{== ")
      elseif elem.classes[1] and elem.classes[1] == "comment-end" then
        local comment = commentholder[elem.attributes["id"]]
        if comment == nil then
          return pandoc.RawInline(FORMAT, "")
        elseif #comment.content == 0 then
          return pandoc.RawInline(FORMAT, " ==}")
        else
          return { pandoc.RawInline(FORMAT, " ==}{>> ") } ..
            comment.content ..
            { pandoc.RawInline(FORMAT, " ("),
            pandoc.RawInline(FORMAT, comment.attributes.author),
            pandoc.RawInline(FORMAT, ") <<}") }
        end
      else
        return nil
      end
    end,
  }
}
