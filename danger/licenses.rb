# SPDX-FileCopyrightText: 2022 Oxhead Alpha
# SPDX-License-Identifier: LicenseRef-MIT-OA

require_relative 'helpers'

def check_licenses
  # Licenses
  # Check that the REUSE license header contains the current year.
  cur_year = Time.new.year
  # Only go over new files; see https://gitlab.com/morley-framework/morley/-/merge_requests/1091
  # for the discussion and rationale for this.
  git.added_files.each do |file|
    File.foreach(file).with_index(1).find do |line, line_num|
      if year_match = line.match(/(^.*SPDX-FileCopyrightText:)\s+(\w+-)?(\w+)\s+(.*)$/)
        head, start, year, holder = year_match.captures
        unless (year == cur_year.to_s)
          markdown(
            ":warning: The year in this license header is outdated, time to update!\n\n",
            file: file, line: line_num
          )
        end
        # either way, we return 'true' to stop looking after the first 'match'
        true
      end
    end
  end
end
