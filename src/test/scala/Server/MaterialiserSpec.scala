package Server

import org.scalatest.{FlatSpec, Matchers}
import sangria.ast.{Field, OperationDefinition}
import sangria.parser.QueryParser
import sangria.renderer.QueryRenderer

import scala.util.Success

class MaterialiserSpec extends FlatSpec with Matchers {
  behavior of "Materialiser"

  it should "connect to HTTPS" in {
    val materialiser = new Materialiser("api.hollowverse.com", ssl = true)
    materialiser.schema
  }

  it should "materialise a schema" in {
    val materialiser = new Materialiser("api.hollowverse.com", ssl = true)
    materialiser.materialise("query IntrospectionQuery {\n  __schema {\n    queryType {\n      name\n    }\n    mutationType {\n      name\n    }\n    subscriptionType {\n      name\n    }\n    types {\n      ...FullType\n    }\n    directives {\n      name\n      description\n      locations\n      args {\n        ...InputValue\n      }\n    }\n  }\n}\n\nfragment FullType on __Type {\n  kind\n  name\n  description\n  fields(includeDeprecated: true) {\n    name\n    description\n    args {\n      ...InputValue\n    }\n    type {\n      ...TypeRef\n    }\n    isDeprecated\n    deprecationReason\n  }\n  inputFields {\n    ...InputValue\n  }\n  interfaces {\n    ...TypeRef\n  }\n  enumValues(includeDeprecated: true) {\n    name\n    description\n    isDeprecated\n    deprecationReason\n  }\n  possibleTypes {\n    ...TypeRef\n  }\n}\n\nfragment InputValue on __InputValue {\n  name\n  description\n  type {\n    ...TypeRef\n  }\n  defaultValue\n}\n\nfragment TypeRef on __Type {\n  kind\n  name\n  ofType {\n    kind\n    name\n    ofType {\n      kind\n      name\n      ofType {\n        kind\n        name\n        ofType {\n          kind\n          name\n          ofType {\n            kind\n            name\n            ofType {\n              kind\n              name\n              ofType {\n                kind\n                name\n              }\n            }\n          }\n        }\n      }\n    }\n  }\n}")
  }

  it should "parse stuff" in {
    val Success(query) = QueryParser.parse("{\n  notablePerson(slug: \"Kelly_Clarkson\") {\n    name\n    labels {\n      text\n    }\n  }\n  notablePeople(first: 10) {\n    edges {\n      node {\n        name\n        slug\n      }\n    }\n  }\n}")
    val Some((_, operation: OperationDefinition)) = query.operations.headOption
    val fieldName = query.operations.headOption.map(o => o._2.selections.head.asInstanceOf[Field]).get.name
    val materialiser = new Materialiser("api.hollowverse.com", ssl = true)
    materialiser.materialise("query IntrospectionQuery {\n  __schema {\n    queryType {\n      name\n    }\n    mutationType {\n      name\n    }\n    subscriptionType {\n      name\n    }\n    types {\n      ...FullType\n    }\n    directives {\n      name\n      description\n      locations\n      args {\n        ...InputValue\n      }\n    }\n  }\n}\n\nfragment FullType on __Type {\n  kind\n  name\n  description\n  fields(includeDeprecated: true) {\n    name\n    description\n    args {\n      ...InputValue\n    }\n    type {\n      ...TypeRef\n    }\n    isDeprecated\n    deprecationReason\n  }\n  inputFields {\n    ...InputValue\n  }\n  interfaces {\n    ...TypeRef\n  }\n  enumValues(includeDeprecated: true) {\n    name\n    description\n    isDeprecated\n    deprecationReason\n  }\n  possibleTypes {\n    ...TypeRef\n  }\n}\n\nfragment InputValue on __InputValue {\n  name\n  description\n  type {\n    ...TypeRef\n  }\n  defaultValue\n}\n\nfragment TypeRef on __Type {\n  kind\n  name\n  ofType {\n    kind\n    name\n    ofType {\n      kind\n      name\n      ofType {\n        kind\n        name\n        ofType {\n          kind\n          name\n          ofType {\n            kind\n            name\n            ofType {\n              kind\n              name\n              ofType {\n                kind\n                name\n              }\n            }\n          }\n        }\n      }\n    }\n  }\n}")

    materialiser.schema.query.fields.filter(f => f.name == fieldName).length should be(1)


    val copied = query.definitions.head match {
      case d: OperationDefinition => {
        d.selections.map(s => {
          query.copy(definitions = Vector(query.definitions.head match {
            case d: OperationDefinition => {
              d.copy(selections = Vector(s))
            }
          }))
        })
      }
    }

    copied.map(c => {
      println("========== query: ==========")
      println(QueryRenderer.render(c))
    })

    query.operations.map(_._2).map(o => o.selections.map(s => {
      println(s);
      println(s.asInstanceOf[Field].name)
    }))
  }
}
