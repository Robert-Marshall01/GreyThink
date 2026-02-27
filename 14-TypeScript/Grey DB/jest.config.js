/** @type {import('ts-jest').JestConfigWithTsJest} */
module.exports = {
    preset: "ts-jest",
    testEnvironment: "node",
    rootDir: ".",
    testMatch: ["<rootDir>/packages/core/tests/**/*.test.ts"],
    moduleNameMapper: {
        "^@grey-db/core/(.*)$": "<rootDir>/packages/core/src/$1",
        "^@grey-db/core$": "<rootDir>/packages/core/src/index",
    },
    transform: {
        "^.+\\.tsx?$": [
            "ts-jest",
            {
                tsconfig: "packages/core/tsconfig.json",
            },
        ],
    },
    testPathIgnorePatterns: ["/node_modules/", "/dist/"],
    collectCoverageFrom: [
        "packages/core/src/**/*.ts",
        "!packages/core/src/index.ts",
    ],
};
