local commentholder

function Span(elem)
  if elem.classes[1] and elem.classes[1] == "insertion" then
    local opener = { pandoc.RawInline(FORMAT, "{++ ") }
    local closer = { pandoc.RawInline(FORMAT, " ++}") }
    return opener .. elem.content .. closer
  elseif
    elem.classes[1] and elem.classes[1] == "deletion" then
    local opener = { pandoc.RawInline(FORMAT, "{-- ") }
    local closer = { pandoc.RawInline(FORMAT, " --}") }
    return opener .. elem.content .. closer
  elseif
    elem.classes[1] and elem.classes[1] == "comment-start" then
    if elem.t == nil then
      return pandoc.RawInline(FORMAT, "")
    end
    commentholder = elem
    return pandoc.RawInline(FORMAT, "{== ")
  elseif
    elem.classes[1] and elem.classes[1] == "comment-end" then
    local opener = { pandoc.RawInline(FORMAT, " ==}{>> ") }
    local closer = { pandoc.RawInline(FORMAT, " ("), pandoc.RawInline(FORMAT, commentholder.attributes.author), pandoc.RawInline(FORMAT, ") <<}")}
    return opener .. commentholder.content .. closer
  else
    return nil
  end
end


-- Addition {++ ++}
-- Deletion {-- --}
-- Substitution {~~ ~> ~~}  # TODO figure out how use CriticMarkup substitution
-- Comment {>> <<}
-- Highlight {== ==}{>> <<}  #TODO figure out how to use CriticMarkup highlighting
