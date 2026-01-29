// =============================================================================
// Grey Multi-Tenant SDK - Kotlin
// build.gradle.kts
//
// Gradle build configuration for the Grey Kotlin SDK.
// Uses gRPC for transport.
// =============================================================================

plugins {
    kotlin("jvm") version "1.9.22"
    id("com.google.protobuf") version "0.9.4"
    `maven-publish`
}

group = "com.grey"
version = "0.1.0"

repositories {
    mavenCentral()
}

val grpcVersion = "1.60.1"
val grpcKotlinVersion = "1.4.1"
val protobufVersion = "3.25.2"
val coroutinesVersion = "1.7.3"

dependencies {
    // Kotlin
    implementation(kotlin("stdlib"))
    
    // Coroutines
    implementation("org.jetbrains.kotlinx:kotlinx-coroutines-core:$coroutinesVersion")
    
    // gRPC
    implementation("io.grpc:grpc-kotlin-stub:$grpcKotlinVersion")
    implementation("io.grpc:grpc-protobuf:$grpcVersion")
    implementation("io.grpc:grpc-netty-shaded:$grpcVersion")
    implementation("com.google.protobuf:protobuf-kotlin:$protobufVersion")
    
    // Testing
    testImplementation(kotlin("test"))
    testImplementation("org.jetbrains.kotlinx:kotlinx-coroutines-test:$coroutinesVersion")
    testImplementation("io.grpc:grpc-testing:$grpcVersion")
}

protobuf {
    protoc {
        artifact = "com.google.protobuf:protoc:$protobufVersion"
    }
    plugins {
        create("grpc") {
            artifact = "io.grpc:protoc-gen-grpc-java:$grpcVersion"
        }
        create("grpckt") {
            artifact = "io.grpc:protoc-gen-grpc-kotlin:$grpcKotlinVersion:jdk8@jar"
        }
    }
    generateProtoTasks {
        all().forEach {
            it.plugins {
                create("grpc")
                create("grpckt")
            }
            it.builtins {
                create("kotlin")
            }
        }
    }
}

kotlin {
    jvmToolchain(17)
}

tasks.test {
    useJUnitPlatform()
}

publishing {
    publications {
        create<MavenPublication>("maven") {
            from(components["java"])
            
            pom {
                name.set("Grey Multi-Tenant SDK")
                description.set("Kotlin SDK for Grey Multi-Tenant platform")
                url.set("https://github.com/grey/grey-kotlin")
            }
        }
    }
}
