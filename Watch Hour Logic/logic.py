def watch_time(total_video_duration, introduction_timestamp, post_credit_timestamp, skipped_parts):
    # Watch Hours Logic
    total_watch_time = total_video_duration - (introduction_timestamp + post_credit_timestamp)

    # Skipped Part Exclusion
    for part in skipped_parts:
        if introduction_timestamp < part < total_timestamp:
            total_watch_time -= part - introduction_timestamp

    # Total Watch Hours
    total_hours = total_watch_time / 3600

    return f"{total_hours:.3f} hours watched" #format

# Example usage
total_timestamp = 8450  # Total video duration
introduction_timestamp = 120  # Introduction duration
post_credit_timestamp = 180  # Post-credit duration
skipped_parts = [600, 900, 250]  # List of skipped parts

result = watch_time(total_timestamp, introduction_timestamp, post_credit_timestamp, skipped_parts)
print(result)
