--[[ 

This felt a bit verbose.  Had to make a function to print out a binary string
for debugging.  Filtering lists in lua is quite explicit but it sure is
straight forward.

]]

function tobinstring(n)
  local s = ''
  while n > 0 do
    local r = math.fmod(n, 2)
    s = tostring(math.floor(r)) .. s
    n = (n-r)/2
  end
  return s
end

function binwidth(n)
  local i = 0
  while n > 0 do
    n = n >> 1
    i = i + 1
  end
  return i
end

function mostfreq(xs, pos)
  local c = 0
  for i,v in ipairs(xs) do
    c = c + ((v >> pos) & 1)
  end
  if c >= #xs/2 then
    return 1
  else
    return 0
  end
end

L = {}
while true do
  local s = io.read("*l")
  if not s then break end
  table.insert(L, tonumber(s, 2))
end

L2 = {table.unpack(L)}
table.sort(L2)
maxwidth = binwidth(L2[#L2])

-- for i,v in ipairs(L) do
-- end

function part1()
  local gamma = 0
  for i = 0, maxwidth-1 do
    local freq = mostfreq(L, i)
    gamma = gamma + (freq << i)
  end

  local mask = 0
  for i = 0, maxwidth-1 do
    mask = mask | (1 << i)
  end
  local epsilon = (gamma ~ mask) & mask
  return gamma * epsilon
end

function part2()
  function f(target)
    local xs = {table.unpack(L)}
    local pos = maxwidth-1
    while #xs > 1 do
      local freq = mostfreq(xs, pos)
      if target == 'least' then
        freq = (~freq & 1)
      end
      local k = #xs
      for i=1, k do
        local idx = k + 1 - i
        local v = xs[idx]
        local currentbit = (v >> pos) & 1
        if currentbit ~= freq then
          table.remove(xs, idx)
        end
      end
      pos = pos - 1
    end
    return xs[1]
  end
  local least = f('least')
  local most = f('most')
  return least * most
end

print(part1())
print(part2())

-- Local Variables:
-- compile-command: "lua day03.lua < sample.txt"
-- End:
