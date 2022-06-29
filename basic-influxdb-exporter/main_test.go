// Copyright 2016 The Prometheus Authors
// Licensed under the Apache License, Version 2.0 (the "License");
// you may not use this file except in compliance with the License.
// You may obtain a copy of the License at
//
// http://www.apache.org/licenses/LICENSE-2.0
//
// Unless required by applicable law or agreed to in writing, software
// distributed under the License is distributed on an "AS IS" BASIS,
// WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
// See the License for the specific language governing permissions and
// limitations under the License.

package main

import (
	"fmt"
	"regexp"
	"sort"
	"strings"
	"testing"
)

const (
	original = "namespace_pod.container:container_cpu_usage_seconds_total:sum_rate"
)

var (
	labels = map[string]string{"name1": "value1", "name2": "value2", "name3": "value3", "name4": "value4"}
	name   = "name"
)

func BenchmarkRegexpReplaceInvalid(b *testing.B) {
	b.ReportAllocs()
	invalidChars := regexp.MustCompile("[^a-zA-Z0-9_]")

	for i := 0; i < b.N; i++ {
		invalidChars.ReplaceAllString(original, "_")
	}
}

func BenchmarkHardcodedReplace(b *testing.B) {
	b.ReportAllocs()
	for i := 0; i < b.N; i++ {
		var newString = original
		ReplaceInvalidChars(&newString)
	}
}

func BenchmarkSprintfArray(b *testing.B) {
	b.ReportAllocs()
	for i := 0; i < b.N; i++ {
		// Calculate a consistent unique ID for the sample.
		labelnames := make([]string, 0, len(labels))
		for k := range labels {
			labelnames = append(labelnames, k)
		}
		sort.Strings(labelnames)
		parts := make([]string, 0, len(labels)*2+1)
		parts = append(parts, name)
		for _, l := range labelnames {
			parts = append(parts, l, labels[l])
		}
		fmt.Sprintf("%q", parts)
	}
}

func BenchmarkStringJoin(b *testing.B) {
	b.ReportAllocs()
	for i := 0; i < b.N; i++ {

		// Calculate a consistent unique ID for the sample.
		labelnames := make([]string, 0, len(labels))
		for k := range labels {
			labelnames = append(labelnames, k)
		}
		sort.Strings(labelnames)
		parts := make([]string, 0, len(labels)*2+1)
		parts = append(parts, name)
		for _, l := range labelnames {
			parts = append(parts, l, labels[l])
		}
		strings.Join(parts, ".")
	}
}
