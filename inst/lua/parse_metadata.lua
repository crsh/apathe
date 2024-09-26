--[[
  Parse metadata fields and append them to the document body

  Contains adapted code from
  https://github.com/pandoc/lua-filters/blob/master/author-info-blocks/author-info-blocks.lua
  Copyright © 2017–2020 Albert Krewinkel
  ]]

local List = require 'pandoc.List'
local utils = require 'pandoc.utils'

local function intercalate(lists, elem)
  local result = List:new{}
  for i = 1, (#lists - 1) do
    result:extend(lists[i])
    result:extend(elem)
  end
  if #lists > 0 then
    result:extend(lists[#lists])
  end
  return result
end

local function author_inline_generator()
  return function (author)
    return List.clone(author.name)
  end
end

--- Generate a list of inlines containing all authors
local function create_authors_inlines(authors, and_str, sups)
  local padded_and_str = List:new{pandoc.Space(), pandoc.Str(and_str), pandoc.Space()}

  local result

  if authors then
    local inlines_generator = author_inline_generator(sups)
    local inlines = List:new(authors):map(inlines_generator)
    local last_author = inlines[#inlines]
    inlines[#inlines] = nil

    
    if #authors > 2 then
      result = intercalate(inlines, {pandoc.Str ",", pandoc.Space()})
      result:extend(List:new{pandoc.Str ","})
    else
      result = intercalate(inlines, {})
    end

    if #authors > 1 then
      result:extend(padded_and_str)
    end

    result:extend(last_author)
  end
  return result
end

--- Create raw LaTeX environments from metadata fields
local function make_latex_envir(name, metadata)
  local data = {table.unpack(metadata)}
  local pandoc_type = data[1].t

  if pandoc_type == "Para" or pandoc_type == "Plain" or pandoc_type == "RawBlock" or pandoc_type == "LineBlock" then
    local envir = List:new{pandoc.Para(pandoc.RawInline("latex", "\\" .. name .. "{"))}
    envir:extend(data)
    envir:extend(List:new{pandoc.Para(pandoc.RawInline("latex", "}"))})
    return envir
  else
    return List:new{pandoc.Para({pandoc.RawInline("latex", "\\" .. name), pandoc.Span(data)})}
  end
end

function Pandoc (document)
  local meta = document.meta
  local blocks = document.blocks

  if not meta.shorttitle or meta.shorttitle == nil or meta.shorttitle[1] == nil then
    meta.shorttitle = pandoc.MetaInlines(List:new{pandoc.Str"SHORT", pandoc.Space(), pandoc.Str"TITLE"})
  end

  -- Append additional Latex environments
  blocks:extend(List:new{pandoc.Para({pandoc.RawInline("latex", "% papaja Lua-filter additions")})})

  blocks:extend(make_latex_envir("shorttitle", meta.shorttitle))

  if meta.authornote and #meta.authornote > 0 then
    blocks:extend(make_latex_envir("authornote", meta.authornote))
  end

  blocks:extend(make_latex_envir("affiliation", {pandoc.RawInline("latex", "\\phantom{0}")}))

  if meta.note and #meta.note > 0 then
    blocks:extend(make_latex_envir("note", meta.note))
  end

  if meta.leftheader and meta.leftheader ~= nil and meta.leftheader[1] ~= nil then
    blocks:extend(make_latex_envir("leftheader", meta.leftheader))
  end

  blocks:extend(List:new{pandoc.Para({pandoc.RawInline("latex", "% End of papaja Lua-filter additions")})})

  if meta.author and #meta.author > 0 then
    meta.author = pandoc.MetaInlines(create_authors_inlines(meta.author, "&", true))
  else
    meta.author = FORMAT == "latex"
      and pandoc.MetaInlines({pandoc.RawInline("latex", "\\phantom{0}")})
      or nil
  end

  return pandoc.Pandoc(blocks, meta)
end
