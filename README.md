# 📰 Newstrust - Decentralized Newsroom

A blockchain-based platform for community-driven journalism with built-in fact-checking incentives and editorial voting mechanisms.

## 🌟 Features

- **Article Submission**: Submit news articles with stake-based commitment
- **Fact-Checking**: Community-driven fact-checking with reputation scoring
- **Editorial Voting**: Democratic editorial decisions through community voting
- **Incentive System**: Reward mechanism for quality fact-checkers and voters
- **Reputation Tracking**: User reputation based on contribution quality

## 🚀 Getting Started

### Prerequisites

- [Clarinet](https://github.com/hirosystems/clarinet) installed
- Stacks wallet for testing

### Installation

1. Clone the repository
2. Navigate to the project directory
3. Run Clarinet commands to interact with the contract

## 📋 Usage

### Submit an Article

```clarity
(contract-call? .Newstrust submit-article "Breaking News Title" "content-hash-here")
```

**Requirements**: Minimum stake of 1 STX

### Fact-Check an Article

```clarity
(contract-call? .Newstrust fact-check-article u1 u85 "evidence-hash")
```

**Parameters**:
- `article-id`: ID of the article to fact-check
- `rating`: Score from 0-100 (credibility rating)
- `evidence-hash`: Hash of supporting evidence

### Cast Editorial Vote

```clarity
(contract-call? .Newstrust editorial-vote u1 true)
```

**Parameters**:
- `article-id`: ID of the article
- `vote`: true for approve, false for reject

### Finalize Article

```clarity
(contract-call? .Newstrust finalize-article u1)
```

Articles can only be finalized after the voting period expires.

## 💰 Economics

### Staking Requirements
- **Article Submission**: 1 STX minimum stake
- **Fact-Checking**: 0.5 STX stake per fact-check
- **Editorial Voting**: 0.25 STX stake per vote

### Rewards
- **Fact-Checkers**: 0.5 STX per approved fact-check
- **Editorial Voters**: 0.3 STX per vote on finalized articles
- **Authors**: Stake returned if article approved

### Approval Criteria
Articles are approved when both:
- Fact-check score ≥ 60%
- Editorial approval rate ≥ 60%

## 🔍 Read-Only Functions

### Get Article Information
```clarity
(contract-call? .Newstrust get-article u1)
```

### Check User Reputation
```clarity
(contract-call? .Newstrust get-user-reputation 'SP1234...)
```



