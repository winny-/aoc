tuple = require"tuple"()

function minCoord(state, coord)
  local min = nil
  for k, _ in pairs(state) do
    if min == nil or k[coord] < min then
      min = k[coord]
    end
  end
  return min
end

function maxCoord(state, coord)
  local max = nil
  for k, v in pairs(state) do
    if max == nil or k[coord] > max then
      max = k[coord]
    end
  end
  return max
end

function dump(state)
  for z = minCoord(state, 3), maxCoord(state, 3) do
    dumpZPlane(state, z)
  end
end

function dumpZPlane(state, z)
  local minX = minCoord(state, 1)
  local maxX = maxCoord(state, 1)
  local minY = minCoord(state, 2)
  local maxY = maxCoord(state, 2)
  for y = minY, maxY do
    for x = minX, maxX do
      local cell = state[tuple(x, y, z)]
      if cell == nil then
        cell = '.'
      end
      io.write(cell)
    end
    io.write("\n")
  end
end


start = {}

x = 0
line = io.stdin:read()
while line ~= nil do
  for y = 1, #line do
    c = line:sub(y, y)
    if c == '#' then
      start[tuple(x,y,0)] = true
    end
  end

  line = io.stdin:read()
  x = x + 1
end

print(tuple(1,2,3))

print(maxCoord(start, 2))
