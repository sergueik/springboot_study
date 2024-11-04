lifecycle_rules = [
  {
    action_type = "Delete"
    conditions = [
      {
        age                   = 30
        created_before        = null
        with_state            = "ARCHIVED"
        matches_storage_class = ["STANDARD"]
      },
      {
        age                   = 60
        created_before        = "2023-01-01"
        with_state            = null
        matches_storage_class = ["NEARLINE"]
      },
    ]
  },
]
