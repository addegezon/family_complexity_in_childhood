function Span(el)
  local color = el.attributes['color']
  -- if no color attribute, return unchanged
  if color == nil then return el end
  
  -- remove color attribute
  el.attributes['color'] = nil
  
  -- encapsulate in LaTeX code
  local latex_begin = pandoc.RawInline('latex', '\\textcolor{'..color..'}{')
  local latex_end = pandoc.RawInline('latex', '}')
  
  -- create a new Span element with modified content
  local modified_content = { latex_begin }
  for _, inline in ipairs(el.content) do
    table.insert(modified_content, inline)
  end
  table.insert(modified_content, latex_end)
  
  return pandoc.Span(modified_content)
end
