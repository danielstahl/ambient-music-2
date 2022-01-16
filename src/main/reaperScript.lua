
function GetChannel(name)
  for i = 0, reaper.GetNumAudioInputs() - 1 do
    local channelName = reaper.GetInputChannelName(i)
    if channelName == name then return i
    end
  end
  return -1
end  

function CreateFolder(index, name)
  reaper.InsertTrackAtIndex(index, false)
  folder = reaper.GetTrack(0, index)
  reaper.GetSetMediaTrackInfo_String(folder, 'P_NAME', name, true)
  reaper.SetMediaTrackInfo_Value( folder, 'I_FOLDERDEPTH',1)
  reaper.SetMediaTrackInfo_Value(folder, 'I_FOLDERCOMPACT', 0)
end

function CreateTrack(index, name, channel, lastInFolder)
  local ch = GetChannel(channel)
  local folderDepth = 0
  if lastInFolder then folderDepth = -1 end
  
  reaper.InsertTrackAtIndex(index, false)
  tr = reaper.GetTrack(0,index)
  reaper.GetSetMediaTrackInfo_String(tr, 'P_NAME', name, true)
  reaper.SetMediaTrackInfo_Value( tr, 'I_RECARM',1)
  reaper.SetMediaTrackInfo_Value( tr, 'I_RECINPUT',1024 + ch)
  reaper.SetMediaTrackInfo_Value( tr, 'I_FOLDERDEPTH',folderDepth)
end

CreateFolder(0, "Low")
CreateTrack(1, "Low 1", "Input 1 (BlackHole 64ch)", false)
CreateTrack(2, "Low 2", "Input 3 (BlackHole 64ch)", false)
CreateTrack(3, "Low 3", "Input 5 (BlackHole 64ch)", false)
CreateTrack(4, "Low 4", "Input 7 (BlackHole 64ch)", true)

CreateFolder(5, "Middle")
CreateTrack(6, "Middle 1", "Input 9 (BlackHole 64ch)", false)
CreateTrack(7, "Middle 2", "Input 11 (BlackHole 64ch)", false)
CreateTrack(8, "Middle 3", "Input 13 (BlackHole 64ch)", false)
CreateTrack(9, "Middle 4", "Input 15 (BlackHole 64ch)", true)

CreateFolder(10, "High")
CreateTrack(11, "High 1", "Input 17 (BlackHole 64ch)", false)
CreateTrack(12, "High 2", "Input 19 (BlackHole 64ch)", false)
CreateTrack(13, "High 3", "Input 21 (BlackHole 64ch)", false)
CreateTrack(14, "High 4", "Input 23 (BlackHole 64ch)", true)




