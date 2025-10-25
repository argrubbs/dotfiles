-- Git status integration for yazi
-- Shows git status in the file manager

local api = require("yazi.api")

local git_status = {
  modified = "M",
  added = "A",
  deleted = "D",
  renamed = "R",
  untracked = "?",
}

return function()
  local files = api.get_files()
  if not files then
    return
  end

  for _, file in ipairs(files) do
    -- Add git icons based on status
    -- This is a simple example - full implementation would require git integration
  end
end
