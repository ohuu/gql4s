// Copyright (c) 2018-2021 by Oli Winks
// This software is licensed under the MIT License (MIT).
// For more information see LICENSE or https://opensource.org/licenses/MIT

package parsers

// Document
val executableDefinition = operationDefinition | fragmentDefinition
val executableDocument   = __ *> (executableDefinition <* __).rep

val definition = executableDefinition | typeSystemDefinitionOrExtension
val document   = __ *> (definition <* __).rep
