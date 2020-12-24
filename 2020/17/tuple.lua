-- An interned 2-tuple type, in pure Lua.
--
-- Via http://lua-users.org/wiki/SimpleTuples
--
do
  -- The constructor makes a tuple function, which has its own
  -- intern table.
        
  local setmetatable = setmetatable

  local function tmaker(a)
    return {__index = function(t, b)
                        local function tuple() return a, b end
                        t[b] = tuple
                        return tuple
                      end,
            __mode = "kv"
           }
  end 
  local meta = {
      __index = function(t, a)
                  local v = setmetatable({}, tmaker(a))
                  t[a] = v
                  return v
                end,
      __mode = "k"
  } 
  return function()
    local intern = setmetatable({}, meta)
    return function(a, b)
      return intern[a][b]
    end
  end
end
