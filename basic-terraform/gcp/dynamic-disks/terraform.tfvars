attach_disks = true
disks = [
  { name = "small-disk", "type" : "pd-standard", "size" : 10, "mode" : "READ_WRITE" },
  { name = "medium-disk", "type" : "pd-balanced", "size" : 10, "mode" : "READ_ONLY" },
]
